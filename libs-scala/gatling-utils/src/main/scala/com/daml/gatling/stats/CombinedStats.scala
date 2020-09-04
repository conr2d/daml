// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling
package stats

import scalaz.\/

case class CombinedStats(
    fixTrades: FixTradeStats,
    schedulerStats: SchedulerStats
) extends Formattable {
  def formatted: String = List(fixTrades.formatted, schedulerStats.formatted).mkString("\n")
}

object CombinedStats {
  implicit val convertCombinedStats: ConvertStats[CombinedStats] =
    new ConvertStats[CombinedStats] {
      val relevantStats =
        ConvertStats[FixTradeStats].relevantStats ++
          ConvertStats[SchedulerStats].relevantStats

      def fromSimulationLog(simulation: SimulationLog): String \/ CombinedStats =
        for {
          fix <- ConvertStats[FixTradeStats].fromSimulationLog(simulation)
          scheduler <- ConvertStats[SchedulerStats].fromSimulationLog(simulation)
        } yield CombinedStats(fix, scheduler)
    }
}
