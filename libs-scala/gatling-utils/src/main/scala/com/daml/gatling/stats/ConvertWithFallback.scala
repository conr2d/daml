// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.gatling.stats

import scalaz.\/

// A or else B
// Allows us to form a non-empty list of types
sealed trait ||[A, B]

/**
  * The goal here is to pass a list of types at which to attempt parsing, and
  * to quit at the first one that works.
  *
  * This type class does an induction over a nonempty type of types expressed using ||.
  * If the list is:
  * <ul>
  *   <li> A, where A is a stats type we can read:
  *        try reading an A.
  *   <li> A || B, where A is a stats type we can read, and we can read B:
  *        try reading an A, else fall back to B
  * <ul>
  *
  * @tparam F should be the least upper bound of the types in the list
  * @tparam R the list of types to attempt parsing at in priority order
  */
sealed trait ConvertWithFallback[F, R] {
  def convert(s: SimulationLog): String \/ F
}

object ConvertWithFallback {
  def apply[F, R](implicit R: ConvertWithFallback[F, R]) = R

  def instance[F, R](f: SimulationLog => String \/ F) =
    new ConvertWithFallback[F, R] { def convert(s: SimulationLog): String \/ F = f(s) }

  implicit def singletonReadStats[F, R <: F: ConvertStats]: ConvertWithFallback[F, R] =
    instance(ConvertStats[R].parse)

  implicit def consReadStats[F, R, S <: F](
      implicit
      R: ConvertWithFallback[F, R],
      S: ConvertStats[S]
  ): ConvertWithFallback[F, R || S] = instance(s => R.convert(s) orElse S.parse(s))
}
