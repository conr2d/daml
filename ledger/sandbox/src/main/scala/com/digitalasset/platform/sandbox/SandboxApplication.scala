// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.sandbox

import java.time.Instant

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import com.digitalasset.api.util.TimeProvider
import com.digitalasset.daml.lf.data.ImmArray
import com.digitalasset.daml.lf.engine.Engine
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.ledger.server.LedgerApiServer.{ApiServices, LedgerApiServer}
import com.digitalasset.platform.sandbox.banner.Banner
import com.digitalasset.platform.sandbox.config.{SandboxConfig, SandboxContext}
import com.digitalasset.platform.sandbox.metrics.MetricsManager
import com.digitalasset.platform.sandbox.services.SandboxResetService
import com.digitalasset.platform.sandbox.stores.ActiveContractsInMemory
import com.digitalasset.platform.sandbox.stores.ledger.ScenarioLoader.LedgerEntryWithLedgerEndIncrement
import com.digitalasset.platform.sandbox.stores.ledger._
import com.digitalasset.platform.sandbox.stores.ledger.sql.SqlStartMode
import com.digitalasset.platform.server.services.testing.TimeServiceBackend
import com.digitalasset.platform.services.time.TimeProviderType
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

object SandboxApplication {
  private val logger = LoggerFactory.getLogger(this.getClass)
  private val asyncTolerance = 30.seconds

  class SandboxServer(actorSystemName: String, config: => SandboxConfig) extends AutoCloseable {

    case class State(
        ledgerId: String,
        port: Int,
        apiServer: LedgerApiServer,
        stopHeartbeats: () => Unit
    )

    case class InfraState(
        actorSystem: ActorSystem,
        materializer: ActorMaterializer,
        metricsManager: MetricsManager) {
      def executionContext: ExecutionContext = materializer.executionContext
    }

    @volatile private var infraState: InfraState = _

    //TODO: get rid of these vars! Stateful resources should be created as vals when the owner object is created.
    @volatile private var server: LedgerApiServer = _
    @volatile private var ledgerId: String = _
    @volatile private var stopHeartbeats: () => Unit = () => ()
    @volatile var port: Int = config.port

    //TODO: why is this exposed???
    def getMaterializer: ActorMaterializer = infraState.materializer

    // We memoize the engine between resets so we avoid the expensive
    // repeated validation of the sames packages after each reset
    private val engine = Engine()

    /** the reset service is special, since it triggers a server shutdown */
    private val resetService: SandboxResetService = new SandboxResetService(
      () => ledgerId,
      () => infraState.executionContext,
      () => resetAndRestartServer()
    )

    // returns with a Future firing when all services have been closed!
    private def resetAndRestartServer(): Future[Unit] = {
      stopHeartbeats()
      //need to run this async otherwise the callback kills the server under the in-flight reset service request!
      Future {
        server.close() // fully tear down the old server.
        buildAndStartServer(SqlStartMode.AlwaysReset)
      }(infraState.executionContext)

      server.servicesClosed()
    }

    @SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes"))
    private def buildAndStartServer(
        startMode: SqlStartMode = SqlStartMode.ContinueIfExists): Unit = {
      implicit val mat = infraState.materializer
      implicit val ec: ExecutionContext = infraState.executionContext
      implicit val mm: MetricsManager = infraState.metricsManager

      ledgerId = config.ledgerIdMode.ledgerId()

      val context = SandboxContext.fromConfig(config)

      val (acs, records, mbLedgerTime) = createInitialState(config, context)

      val (timeProvider, timeServiceBackendO: Option[TimeServiceBackend]) =
        (mbLedgerTime, config.timeProviderType) match {
          case (None, TimeProviderType.WallClock) => (TimeProvider.UTC, None)
          case (None, _) =>
            val ts = TimeServiceBackend.simple(Instant.EPOCH)
            (ts, Some(ts))
          case (Some(ledgerTime), _) =>
            val ts = TimeServiceBackend.simple(ledgerTime)
            (ts, Some(ts))
        }

      val (ledgerType, ledger) = config.jdbcUrl match {
        case None =>
          ("in-memory", Ledger.metered(Ledger.inMemory(ledgerId, timeProvider, acs, records)))
        case Some(jdbcUrl) =>
          val ledgerF = Ledger.postgres(
            jdbcUrl,
            ledgerId,
            timeProvider,
            records,
            config.commandConfig.maxCommandsInFlight * 2, // we can get commands directly as well on the submission service
            startMode
          )

          val ledger = Try(Await.result(ledgerF, asyncTolerance)).fold(t => {
            val msg = "Could not start PostgreSQL persistence layer"
            logger.error(msg, t)
            sys.error(msg)
          }, identity)

          (s"sql", Ledger.metered(ledger))
      }

      val ledgerBackend = new SandboxLedgerBackend(ledger)

      stopHeartbeats = scheduleHeartbeats(timeProvider, ledger.publishHeartbeat)
      server = LedgerApiServer(
        (am: ActorMaterializer, esf: ExecutionSequencerFactory) =>
          ApiServices
            .create(
              config,
              ledgerBackend,
              engine,
              timeProvider,
              timeServiceBackendO
                .map(
                  TimeServiceBackend.withObserver(
                    _,
                    ledger.publishHeartbeat
                  )))(am, esf)
            .withServices(List(resetService)),
        port,
        config.address,
        config.tlsConfig.flatMap(_.server)
      )

      // NOTE(JM): Re-use the same port after reset.
      port = server.port

      Banner.show(Console.out)
      logger.info(
        "Initialized sandbox version {} with ledger-id = {}, port = {}, dar file = {}, time mode = {}, ledger = {}, daml-engine = {}",
        BuildInfo.Version,
        ledgerId,
        port.toString,
        config.damlPackageContainer: AnyRef,
        config.timeProviderType,
        ledgerType
      )
    }

    //TODO: make this private!
    def start(): Unit = {
      val actorSystem = ActorSystem(actorSystemName)
      infraState = InfraState(actorSystem, ActorMaterializer()(actorSystem), MetricsManager())

      buildAndStartServer()
    }

    override def close(): Unit = {
      stopHeartbeats()
      Option(infraState).foreach { is =>
        is.materializer.shutdown()
        Await.result(is.actorSystem.terminate(), asyncTolerance)
        is.metricsManager.close()
      }
      //TODO: stop ledger backend here instead!
    }
  }

  def apply(config: => SandboxConfig): SandboxServer =
    new SandboxServer(
      "sandbox",
      config
    )

  private def scheduleHeartbeats(timeProvider: TimeProvider, onTimeChange: Instant => Future[Unit])(
      implicit mat: ActorMaterializer,
      ec: ExecutionContext) =
    timeProvider match {
      case timeProvider: TimeProvider.UTC.type =>
        val interval = 1.seconds
        logger.debug(s"Scheduling heartbeats in intervals of {}", interval)
        val cancelable = Source
          .tick(0.seconds, interval, ())
          .mapAsync[Unit](1)(
            _ => onTimeChange(timeProvider.getCurrentTime)
          )
          .to(Sink.ignore)
          .run()
        () =>
          val _ = cancelable.cancel()
      case _ =>
        () =>
          ()
    }

  // if requested, initialize the ledger state with the given scenario
  private def createInitialState(config: SandboxConfig, context: SandboxContext)
    : (ActiveContractsInMemory, ImmArray[LedgerEntryWithLedgerEndIncrement], Option[Instant]) =
    config.scenario match {
      case None => (ActiveContractsInMemory.empty, ImmArray.empty, None)
      case Some(scenario) =>
        val (acs, records, ledgerTime) =
          ScenarioLoader.fromScenario(context.packageContainer, scenario)
        (acs, records, Some(ledgerTime))
    }

}
