// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling
package stats

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.daml.gatling.stats.ConvertStats.StatsKey
import com.daml.scalautil.Statement.discard
import scalaz.\/

/*
 * Provides the ability to parse from Gatling's JSON format, given a set of relevant stats
 * to look for and a way of handling them.
 */
trait ConvertStats[StatsT] {
  def relevantStats: List[StatsKey]

  def fromSimulationLog(simulation: SimulationLog): String \/ StatsT

  def parse(log: SimulationLog): String \/ StatsT = fromSimulationLog(log)
}

object ConvertStats {
  type StatsKey = String
  def apply[S](implicit R: ConvertStats[S]): ConvertStats[S] = R
}

object SimulationLogConvertSyntax extends SimulationLogConvertSyntax

trait SimulationLogConvertSyntax {
  implicit class SimulationLogConvertOps(val log: SimulationLog) {
    def parseStats[S: ConvertStats] = ConvertStats[S].parse(log)
    def parseFormattableStats[S](implicit S: ConvertWithFallback[Formattable, S]) = S.convert(log)
    def readAnyStats: String \/ Formattable =
      convertFormattableStats[CombinedStats || FixTradeStats || SchedulerStats]

    def convertFormattableStats[R](implicit R: ConvertWithFallback[Formattable, R]) =
      R.convert(log)

    def convertWrap[A: ConvertStats](s: SimulationLog, f: A => Stats) =
      ConvertStats[A].parse(s).map(f)

    def convertOtherStats =
      convertWrap(log, Stats.Combined)
        .orElse(convertWrap(log, Stats.FixTrade))
        .orElse(convertWrap(log, Stats.Scheduler))

    def convertStats[S: ConvertStats] =
      ConvertStats[S].parse(log)

    /**
      * Will write a summary.csv given a Gatling result directory.
      * @param targetDirectory the directory where the summary.csv will be created.
      */
    def writeSummary(targetDirectory: File): Unit = {
      discard {
        Files.write(
          new File(targetDirectory, "summary.csv").toPath,
          log.toCsv.getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
