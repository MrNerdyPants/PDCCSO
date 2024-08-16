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

package cec2013.util

/** Search space related definitions
 *
 * Points, Elements (vectors), Dimensions, limits, ...
 */
object SearchSpace {

  type Point = Double // a point in the search space
  val BAD_POINT = Double.MaxValue // used to indicate null or not valid points

  type Element = Vector[Point] // a vector in the search space

  object Element {
    def apply(p: Point*): Element = p // factory method to build an element from a sequence of points
  }

  // implicit conversions
  implicit def toElement(v: Vector[Point]): Element = v

  implicit def toElement(s: Seq[Point]): Element = s.toVector

  type FitnessFunction = (Element) => Double // a fitness function type

  /** Search space boundaries ------------------------------------------------------------------------- */

  type Bound = Element // a bounding vector in the search space
  val EMPTY_BOUND = Element() // an empty boundary

  // dimension and limits of the search space
  case class SearchSpaceParameters(dim: Int,
                                   lower: Point,
                                   upper: Point)

  /**
   * Function type to bound a Point to upper and lower limits:
   * (point, low bound, upper bound) => bounded point
   */
  type BoundFunction = (Point, Point, Point) => Point

  /** Bounding functions */
  object BoundFunction {
    /**
     * Scale a sample inside the lower and upper limits
     *
     * @return a bounding function that scales an uniform sample in [0.0, 1.0) inside search space limits
     * */
    def scale: BoundFunction = (x: Point, l: Point, u: Point) => l + x * (u - l)
  }

  /**
   * Implicit to add a bound method to Element
   * */
  implicit def toBoundedElement(e: Element) = new {
    /** bound an element to search space limits using the given bounding function
     *
     * @param f      the bounding function
     * @param params the search space dimension and limits
     * @return the element bounded to search space limits
     */
    def bound(f: BoundFunction)(params: SearchSpaceParameters): Element = {
      val lbound = Vector.fill(params.dim)(params.lower) // lower bound
      val ubound = Vector.fill(params.dim)(params.upper) // upper bound
      (e, lbound, ubound).zipped.map((x, l, u) => f(x, l, u))
    }
  }

}
