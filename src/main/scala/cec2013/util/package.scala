/*
 * Copyright (C) 2022  Xoán C. Pardo
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package cec2013

import scala.concurrent.duration.Deadline

package object util {

  /** Random number generators
   *
   *  wrappers around Apache Commons Math Random library
   */
  object Random {
    import org.apache.commons.math3.random.{MersenneTwister, SynchronizedRandomGenerator, RandomDataGenerator}
    import org.apache.commons.math3.distribution.{UniformRealDistribution, UniformIntegerDistribution}

    private lazy val seed = (new RandomDataGenerator).nextSecureLong(Long.MinValue, Long.MaxValue)
    private lazy val random = new SynchronizedRandomGenerator(new MersenneTwister(seed))
    lazy val generator = new RandomDataGenerator(random)
    lazy val reals = new UniformRealDistribution(random, 0.0, 1.0)
    lazy val integers = new UniformIntegerDistribution(random, 0, 1) // Binary uniform distribution
  }

  /** execution time
   *
   *  The execution time of any code can be measured using: "duration { <code> }"
   */
  def duration[R](code: => R) = {  // measure code execution time
    val t1 = Deadline.now
    (code, Deadline.now - t1)
  }

  def duration(code: => Unit) = {  // measure code execution time
    val t1 = Deadline.now
    code
    Deadline.now - t1
  }

}
