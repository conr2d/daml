// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling
package stats

// generic container for counted Gatling things
// the T parameter is often Int, sometimes Double, Long for requestCount
case class Count[T: Numeric](total: T, ok: T, ko: T) {

  def formatted(name: String) =
    s"> %-${available}s%8s (OK=%-6s KO=%-6s)".format(
      name,
      printN(total),
      printN(ok),
      printN(ko)
    )
}
