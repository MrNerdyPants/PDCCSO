import cec2013.util.DataFileReader.{Permutations, RotationMatrix}
import cec2013.util.SearchSpace.Element

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

package object cec2013 {

  val twoPi = 2 * math.Pi

  // search space dimensions
  val dim = 25

  /**
   *  Implicit class to add transformation operations to an Element
   */
  implicit class ElementTransformations(val elem: Element) {

    // x - xopt (shift)
    def shift(elemOpt: Element): Element = {
      assert(elemOpt.length >= elem.length)
      (elem, elemOpt).zipped.map((x, xOpt) => x - xOpt) // if elemOpt is longer than elem, remaining points are ignored by zipped
    }

    // Tosz (oscillation)
    def oscillation(): Element = {
      def hat(x: Double): Double = if (x == 0.0) 0.0 else math.log(math.abs(x))
      def c1(x: Double): Double = if (x > 0.0) 10.0 else 5.5
      def c2(x: Double): Double = if (x > 0.0) 7.9 else 3.1
      elem.map(x => {
        val h = hat(x)
        math.signum(x) * math.exp(h + 0.049 * (math.sin(c1(x) * h) + math.sin(c2(x) * h)))
      })
    }

    private val dim: Double = elem.length - 1 // subtract 1 here to avoid doing it for every position

    // Tasy (asymmetry)
    def asymmetry(beta: Double): Element = elem.zipWithIndex.map(e => // e = (x, index)
      if (e._1 > 0.0) math.pow(e._1, 1 + beta * e._2 / dim * math.sqrt(e._1)) else e._1)

    // lambda (ill conditioning)
    def illConditioning(alpha: Int): Element = elem.zipWithIndex.map(e => // e = (x, index)
      e._1 * math.pow(alpha, 0.5 * e._2 / dim))

    // permute element: change positions using the indexes in p
    def permute(p: Permutations): Element = {
      assert(p.size == elem.size)
      // TODO: assert indexes in p are all different and all are in [0, dim-1]
      p.map(i => elem(i))
    }

    // rotate element: element * rotation matrix
    def rotate(rm: RotationMatrix): Element = {
      elem.indices.map(i =>
        (elem, rm(i)).zipped.foldRight(0.0)((x, acc) => acc + (x._1 * x._2))).toVector // x = (vi, rj)
    }
  }

}
