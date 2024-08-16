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

import util.SearchSpace.{Element, FitnessFunction}

/** Implementations of the basic functions
 *
 * They have been implemented to mimic their C++ counterparts
 * in order to reduce the differences in rounding errors to a minimum
 */


/**
 * The sphere function
 */
object Sphere {
  def apply(): FitnessFunction = { e: Element =>
    e.foldRight(0.0)((x, acc) => acc + math.pow(x, 2))
  }
}

/**
 * The elliptic function
 */
object Elliptic {
  def apply(): FitnessFunction = { e: Element =>
    val dim: Double = e.length - 1
    e.foldLeft(0, 0.0)((acc, x) => // acc = (index, sum)
      (acc._1 + 1, acc._2 + (math.pow(1.0e6, acc._1 / dim) * x * x)))._2 // return the value accumulated in the second position
  }
}

/**
 * The Rastrigin's function
 */
object Rastrigin {
  def apply(): FitnessFunction = { e: Element =>
    e.foldRight(0.0)((x, acc) =>
      acc + (x * x - 10.0 * math.cos(twoPi * x) + 10.0))
  }
}

/**
 * The Ackley’s function
 */
object Ackley {
  def apply(): FitnessFunction = { e: Element =>
    val dim: Double = e.length
    val sum = e.foldRight(0.0, 0.0)((x, acc) => // acc = (sum1, sum2)
      (acc._1 + x * x, acc._2 + math.cos(twoPi * x)))
    -20.0 * math.exp(-0.2 * math.sqrt(sum._1 / dim)) - math.exp(sum._2 / dim) + 20.0 + math.E
  }
}

/**
 * The Schwefel’s Problem 1.2 function
 */
object Schwefel {
  def apply(): FitnessFunction = { e: Element =>
    e.foldLeft(0.0, 0.0)((acc, x) => { // acc = (sum1, sum2)
      val sum1 = acc._1 + x
      (sum1, acc._2 + sum1 * sum1)
    })._2 // return the value accumulated in the second position
  }
}

/**
 * The Rosenbrock’s function
 */
object Rosenbrock {
  def apply(): FitnessFunction = { e: Element =>
    (e.dropRight(1), e.tail).zipped.foldRight(0.0)((x, acc) => { // x = (e[i], e[i+1]), each of the first D-1 positions is zipped with its next one
      val t = x._1 * x._1 - x._2
      acc + 100.0 * t * t + math.pow(x._1 - 1.0, 2)
    })
  }
}