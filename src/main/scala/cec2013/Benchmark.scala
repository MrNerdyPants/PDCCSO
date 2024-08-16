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

import cec2013.util.DataFileReader.CDataFileOps
import cec2013.util.SearchSpace.{Element, FitnessFunction, SearchSpaceParameters}

import scala.collection.immutable.HashMap

object Benchmark {

  /**
   * Base definitions for all benchmark functions
   */
  trait BenchmarkFunctionInfo {
    def id: String // function id

    def info: SearchSpaceParameters // search space dim and limits
  }

  abstract class BenchmarkFunction(_id: => String, _info: => SearchSpaceParameters)
    extends FitnessFunction
      with BenchmarkFunctionInfo
      with CDataFileOps {
    override def id = _id

    override def info = _info

    protected lazy val xopt = readOptimal(id) // optimal solution is read lazily from cdatafiles
  }

  /** Trait with definitions for functions
   * with separable and non-separable components
   */
  trait Subcomponents {
    self: BenchmarkFunction =>

    // cdatafiles are read lazily
    protected lazy val p = readPermutations(id).map(p => p - 1) // permutation indexes are shifted by 1 to start at 0
    protected lazy val s = readSubcomponents(id)
    protected lazy val w = readWeights(id)
    protected lazy val r = HashMap(
      (25, readRotation(id, 25)),
      (50, readRotation(id, 50)),
      (100, readRotation(id, 100)))

    // cumulative sum of the s values
    private lazy val c = (1 to s.size).map(i => s.take(i).sum).+:(0)

    /** extract and rotate the non-separable subcomponents of an Element
     *
     * @param y the element
     * @param m for functions with overlapping subcomponentes, the overlapping slice
     * @return a Vector with the non-separable subcomponents
     */
    def nonSeparable(y: Element, m: Int = 0): Vector[Element] = (1 to s.size).map(i =>
      y.slice(c(i - 1) - (i - 1) * m, c(i) - (i - 1) * m).rotate(r(s(i - 1)))).toVector

    /** extract and rotate the non-separable conflicting subcomponents of an Element
     *
     * @param x the element
     * @param m for functions with overlapping subcomponentes, the overlapping slice
     * @return a Vector with the non-separable conflicting subcomponents
     */
    def nonSeparableConflicting(x: Element, m: Int): Vector[Element] = (1 to s.size).map(i =>
      (x.slice(c(i - 1) - (i - 1) * m, c(i) - (i - 1) * m), xopt.slice(c(i - 1), c(i))).zipped.map((xi, xiopt) => xi - xiopt).rotate(r(s(i - 1)))).toVector

    /** extrat the separable subcomponent of an Element
     *
     * @param y the Element
     * @return the separable subcomponent
     */
    def separable(y: Element): Element = y.drop(s.sum)

    /** evaluates a function with non-separable subcomponents
     *
     * @param f  the function
     * @param zi a Vector with the non-separable subcomponents
     * @return the fitness value
     */
    def evaluate(f: FitnessFunction)(zi: Vector[Element]): Double =
      (zi, w).zipped.map((zi, wi) => wi * f(zi)).sum

    /** evaluates a function with non-separable and separable subcomponents
     *
     * @param f1 the basic function to evaluate the non-separable subcomponents
     * @param f2 the basic function to evaluate the separable subcomponent
     * @param zi a Vector with the non-separable subcomponents
     * @param zs the separable subcomponent
     * @return the fitness value
     */
    def evaluate(f1: FitnessFunction, f2: FitnessFunction)(zi: Vector[Element], zs: Element): Double =
      evaluate(f1)(zi) + f2(zs)
  }

  // common search space dimension and limits
  private val params5 = SearchSpaceParameters(dim, -5, 5)
  private val params32 = SearchSpaceParameters(dim, -32, 32)
  private val params100 = SearchSpaceParameters(dim, -100.0, 100.0)

  /** The benchmark functions ---------------------------------------------------------------------------------- */

  /**
   * Shifted Elliptic Function
   */
  class F1 extends BenchmarkFunction(F1.id, F1.info) {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      val z = e.shift(xopt).oscillation()
      Elliptic()(z)
    }
  }

  object F1 extends BenchmarkFunctionInfo {
    override def id = "F1"

    override def info = params100

    def apply(): BenchmarkFunction = new F1()
  }

  /**
   * Shifted Rastrigin’s Function
   */
  class F2 extends BenchmarkFunction(F2.id, F2.info) {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      val z = e.shift(xopt).oscillation().asymmetry(0.2).illConditioning(10)
      Rastrigin()(z)
    }
  }

  object F2 extends BenchmarkFunctionInfo {
    override def id = "F2"

    override def info = params5

    def apply(): BenchmarkFunction = new F2()
  }

  /**
   * Shifted Ackley’s Function
   */
  class F3 extends BenchmarkFunction(F3.id, F3.info) {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      val z = e.shift(xopt).oscillation().asymmetry(0.2).illConditioning(10)
      Ackley()(z)
    }
  }

  object F3 extends BenchmarkFunctionInfo {
    override def id = "F3"

    override def info = params32

    def apply(): BenchmarkFunction = new F3()
  }

  /**
   * 7-nonseparable, 1-separable Shifted and Rotated Elliptic Function
   */
  class F4
    extends BenchmarkFunction(F4.id, F4.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 7, s"Wrong number of separable parts in $id") // separable parts should be 7
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable and separable subcomponents with the Elliptic function
      evaluate(Elliptic(), Elliptic())(
        nonSeparable(y).map(_.oscillation()),
        separable(y).oscillation())
    }
  }

  object F4 extends BenchmarkFunctionInfo {
    override def id = "F4"

    override def info = params100

    def apply(): BenchmarkFunction = new F4()
  }

  /**
   * 7-nonseparable, 1-separable Shifted and Rotated Rastrigin’s Function
   */
  class F5
    extends BenchmarkFunction(F5.id, F5.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 7, s"Wrong number of separable parts in $id") // separable parts should be 7
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable and separable subcomponents with the Rastrigin function
      evaluate(Rastrigin(), Rastrigin())( // evaluate the function
        nonSeparable(y).map(_.oscillation().asymmetry(0.2).illConditioning(10)),
        separable(y).oscillation().asymmetry(0.2).illConditioning(10))
    }
  }

  object F5 extends BenchmarkFunctionInfo {
    override def id = "F5"

    override def info = params5

    def apply(): BenchmarkFunction = new F5()
  }

  /**
   * 7-nonseparable, 1-separable Shifted and Rotated Ackley’s Function
   */
  class F6
    extends BenchmarkFunction(F6.id, F6.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 7, s"Wrong number of separable parts in $id") // separable parts should be 7
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable and separable subcomponents with the Ackley function
      evaluate(Ackley(), Ackley())( // evaluate the function
        nonSeparable(y).map(_.oscillation().asymmetry(0.2).illConditioning(10)),
        separable(y).oscillation().asymmetry(0.2).illConditioning(10))
    }
  }

  object F6 extends BenchmarkFunctionInfo {
    override def id = "F6"

    override def info = params32

    def apply(): BenchmarkFunction = new F6()
  }

  /**
   * 7-nonseparable, 1-separable Shifted Schwefel’s Function
   */
  class F7
    extends BenchmarkFunction(F7.id, F7.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 7, s"Wrong number of separable parts in $id") // separable parts should be 7
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable subcomponents with the Schwefel function and the separable subcomponent with the Sphere function
      evaluate(Schwefel(), Sphere())(
        nonSeparable(y).map(_.oscillation().asymmetry(0.2)),
        separable(y) /*.oscillation().asymmetry(0.2)*/) // implemented without oscillation and asymmetry as in the original C++ code,
      // but this is different to what's explained in the Li et al. (2013) technical report
    }
  }

  object F7 extends BenchmarkFunctionInfo {
    override def id = "F7"

    override def info = params100

    def apply(): BenchmarkFunction = new F7()
  }

  /**
   * 20-nonseparable Shifted and Rotated Elliptic Function
   */
  class F8
    extends BenchmarkFunction(F8.id, F8.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 20, s"Wrong number of separable parts in $id") // separable parts should be 20
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable subcomponents with the Elliptic function
      evaluate(Elliptic())(nonSeparable(y).map(_.oscillation()))
    }
  }

  object F8 extends BenchmarkFunctionInfo {
    override def id = "F8"

    override def info = params100

    def apply(): BenchmarkFunction = new F8()
  }

  /**
   * 20-nonseparable Shifted and Rotated Rastrigin’s Function
   */
  class F9
    extends BenchmarkFunction(F9.id, F9.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 20, s"Wrong number of separable parts in $id") // separable parts should be 20
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable subcomponents with the Rastrigin function
      evaluate(Rastrigin())(
        nonSeparable(y).map(_.oscillation().asymmetry(0.2).illConditioning(10)))
    }
  }

  object F9 extends BenchmarkFunctionInfo {
    override def id = "F9"

    override def info = params5

    def apply(): BenchmarkFunction = new F9()
  }

  /**
   * 20-nonseparable Shifted and Rotated Ackley’s Function
   */
  class F10
    extends BenchmarkFunction(F10.id, F10.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 20, s"Wrong number of separable parts in $id") // separable parts should be 20
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable subcomponents with the Ackley function
      evaluate(Ackley())(
        nonSeparable(y).map(_.oscillation().asymmetry(0.2).illConditioning(10)))
    }
  }

  object F10 extends BenchmarkFunctionInfo {
    override def id = "F10"

    override def info = params32

    def apply(): BenchmarkFunction = new F10()
  }

  /**
   * 20-nonseparable Shifted Schwefel’s Function
   */
  class F11
    extends BenchmarkFunction(F11.id, F11.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      assert(s.size == 20, s"Wrong number of separable parts in $id") // separable parts should be 20
      val y = e.shift(xopt).permute(p)
      // evaluate non-separable subcomponents with the Schwefel function
      evaluate(Schwefel())(
        nonSeparable(y).map(_.oscillation().asymmetry(0.2)))
    }
  }

  object F11 extends BenchmarkFunctionInfo {
    override def id = "F11"

    override def info = params100

    def apply(): BenchmarkFunction = new F11()
  }

  /**
   * Shifted Rosenbrock’s Function
   */
  class F12 extends BenchmarkFunction(F12.id, F12.info) {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      val z = e.shift(xopt)
      Rosenbrock()(z)
    }
  }

  object F12 extends BenchmarkFunctionInfo {
    override def id = "F12"

    override def info = params100

    def apply(): BenchmarkFunction = new F12()
  }

  /**
   * Shifted Schwefel’s Function with Conforming Overlapping Subcomponents
   */
  class F13
    extends BenchmarkFunction(F13.id, F13.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == 905 || e.size == dim, s"Wrong dimension in $id") // dimension must be 1000 but reduced size is also accepted
      assert(s.size == 20, s"Wrong number of separable parts in $id") // separable parts should be 20
      val y = e.take(905).shift(xopt).permute(p) // only the first 905 dimensions are considered in transformations due to overlapping
      // evaluate non-separable subcomponents with the Schwefel function
      evaluate(Schwefel())(
        nonSeparable(y, 5).map(_.oscillation().asymmetry(0.2)))
    }
  }

  object F13 extends BenchmarkFunctionInfo {
    override def id = "F13"

    override def info = params100

    def apply(): BenchmarkFunction = new F13()
  }

  /**
   * Shifted Schwefel’s Function with Conflicting Overlapping Subcomponents
   */
  class F14
    extends BenchmarkFunction(F14.id, F14.info)
      with Subcomponents {
    override def apply(e: Element): Double = {
      assert(e.size == 905 || e.size == dim, s"Wrong dimension in $id") // dimension must be 1000 but reduced size is also accepted
      assert(s.size == 20, s"Wrong number of separable parts in $id") // separable parts should be 20
      val x = e.take(905).permute(p) // only the first 905 dimensions are considered in transformations due to overlapping
      // evaluate non-separable conflicting subcomponents with the Schwefel function
      // Note that in this function the individual subcomponents are shifted in the nonSeparableConflicting method
      evaluate(Schwefel())(
        nonSeparableConflicting(x, 5).map(_.oscillation().asymmetry(0.2)))
    }
  }

  object F14 extends BenchmarkFunctionInfo {
    override def id = "F14"

    override def info = params100

    def apply(): BenchmarkFunction = new F14()
  }

  /**
   * Shifted Schwefel’s Function
   */
  class F15 extends BenchmarkFunction(F15.id, F15.info) {
    override def apply(e: Element): Double = {
      assert(e.size == dim, s"Wrong dimension in $id") // dimension must be 1000
      val z = e.shift(xopt).oscillation().asymmetry(0.2)
      Schwefel()(z)
    }
  }

  object F15 extends BenchmarkFunctionInfo {
    override def id = "F15"

    override def info = params100

    def apply(): BenchmarkFunction = new F15()
  }

  /** The benchmark constructor  ---------------------------------------------------------------------------------- */

  object LSGO2013 {
    def apply(): Vector[BenchmarkFunction] =
      Vector(F1(), F2(), F3(), F4(), F5(), F6(), F7(), F8(), F9(), F10(), F11(), F12(), F13(), F14(), F15())
  }

}
