// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling.stats

// generic container for Gatling statistics things
case class StatGroup(name: String, count: Int, percentage: Double) {
  def formatted = s"> %-${available}s%8s (%3s%%)".format(name, count, printN(percentage))
}
