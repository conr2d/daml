// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling.stats

import com.daml.gatling.stats.SimulationLog.RequestTypeStats
import scalaz._
import Scalaz._

// the actually-interesting statistics of a FixTrade simulation
// We need to get these numbers out of a json file produced by the file writer.
case class FixTradeStats(
    syncResponses: RequestTypeStats,
    asyncResponses: RequestTypeStats
) extends Formattable {
  def formatted: String =
    "%s\n%s".format(
      syncResponses.formatted("Synchronous Responses (AE)"),
      asyncResponses.formatted("Asynchronous Responses (AR)")
    )
}

object FixTradeStats {

  implicit val convertFixStats = new ConvertStats[FixTradeStats] {
    val relevantStats = List("fix-trade-sync", "fix-trade-async")

    def fromSimulationLog(simulation: SimulationLog): String \/ FixTradeStats =
      relevantStats.map(simulation.requestsByType.get) match {
        case List(Some(sync), async) =>
          FixTradeStats(sync, async.getOrElse(mzero[RequestTypeStats])).right
        case List(None, None) =>
          "FixTradeStats: Unable to find valid statistics".left
      }
  }
}
