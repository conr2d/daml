// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling.stats

import com.daml.gatling.stats.SimulationLog.RequestTypeStats
import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

case class SchedulerStats(
    sync: Count[Int],
    accp: Option[Count[Int]] = None,
    comp: Option[Count[Int]] = None
) extends Formattable {
  def formatted: String =
    List(
      "=" * lineLength,
      subtitle("Scheduler message"),
      msgLine("Acknowledgement", sync.some),
      msgLine("ACCEPTED message", accp),
      msgLine("COMPLETED message", comp),
      "=" * lineLength
    ).mkString("\n")

  private def msgLine(name: String, mbCount: Option[Count[Int]]) =
    mbCount
      .map { cc =>
        s"> %-${available}s  %8s ms   (   %-8s )".format(
          name,
          cc.total,
          if (cc.ok == 0) "FAILED" else "OK")
      }
      .getOrElse(s"> %-${available}s     %8s   (not received)".format(name, "-"))
}

object SchedulerStats {

  implicit val convertSchedulerStats = new ConvertStats[SchedulerStats] {
    val relevantStats = List("scheduler-sync", "scheduler-ACCP", "scheduler-COMP")

    def fromSimulationLog(simulation: SimulationLog): String \/ SchedulerStats =
      if (simulation.requestsByType
          .filterKeys(relevantStats contains _)
          .values
          .toList
          .any(!isSingletonStat(_)))
        s"SchedulerStats: found invalid (non-singleton) stats in $simulation".left
      else {
        val allStats = relevantStats.map(simulation.requestsByType.get)

        allStats match {
          case List(None, None, None) =>
            "SchedulerStats: no values available".left
          case List(Some(sync), acct, comp) =>
            SchedulerStats(
              sync.attribute(_.percentile(0.0)),
              acct.map(_.attribute(_.percentile(0.0))),
              comp.map(_.attribute(_.percentile(0.0)))
            ).right
        }
      }
    private def isSingletonStat(stats: RequestTypeStats): Boolean = stats.count == 1L
  }
}
