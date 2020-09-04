// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling.stats

sealed trait Stats extends Formattable {
  def stats: Formattable
  final def formatted = stats.formatted
}

object Stats {
  final case class FixTrade(stats: FixTradeStats) extends Stats
  final case class Scheduler(stats: SchedulerStats) extends Stats
  final case class Combined(stats: CombinedStats) extends Stats
}
