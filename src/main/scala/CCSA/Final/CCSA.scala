package CCSA.Final

/**
 *
 * @project PDCCSO
 * @file CCSA
 * @author Kashan Asim on 12/09/2024.
 *
 */

import baka.testfunction.matchTest

import scala.util.Random

class CCSA(val D: Int, val MaxIt: Int, val PopSize: Int, val lu: Array[Array[Double]]) {
  private val rand = new Random()
  private val X: Array[Array[Double]] = Array.fill(PopSize, D)(0.0)
  private var m: Array[Array[Double]] = Array.fill(PopSize, D)(0.0)
  private var f: Array[Double] = Array.fill(PopSize)(Double.MaxValue)
  private var fbest: Array[Double] = Array.fill(PopSize)(Double.MaxValue)
  private var m_GbestPos: Array[Double] = Array.fill(D)(0.0)
  private var m_GbestVal: Double = Double.MaxValue
  private val Convergence: Array[Double] = Array.ofDim[Double](MaxIt)

  // Initialize population
  (0 until PopSize).foreach { i =>
    (0 until D).foreach { j =>
      X(i)(j) = lu(0)(j) + rand.nextDouble() * (lu(1)(j) - lu(0)(j))
    }
  }

  m = X.map(_.clone)
  f = Fitness(X)
  fbest = f.clone()
  m_GbestVal = f.min
  m_GbestPos = m(f.indexOf(m_GbestVal)).clone
  Convergence(0) = m_GbestVal

  // Main optimization loop
  (1 until MaxIt).foreach { t =>
    val fl = 2.02 - t * (1.08 / MaxIt)

    (0 until PopSize).foreach { i =>
      val Out = Generating_Neighborhood(i, X, m, f, fbest).transpose
      val Mu = mean(Out)

      val N = Out.map(row => row.zip(Mu).map { case (x, mu) => if (x < mu) 1 else 0 })
      val non_N = Out.map(row => row.zip(Mu).map { case (x, mu) => if (x > mu) 1 else 0 })

      val Neigh = N.indices.filter(row => N(row).contains(1))
      val Nun_Neigh = non_N.indices.filter(row => non_N(row).contains(1))

      val Local = if (Neigh.nonEmpty) Some(Neigh(rand.nextInt(Neigh.size))) else None
      val Global = if (Nun_Neigh.nonEmpty) Some(Nun_Neigh.minBy(fbest(_))) else None

      (Local, Global) match {
        case (Some(local), Some(global)) =>
          X(i) = if (fbest(local) < fbest(global)) nls(X(i), m(local), f(i), fbest(i), lu)
          else ngs(X(i), X, m(global), f(i), fbest(i), lu, fl)

        case (Some(local), None) =>
          X(i) = nls(X(i), m(local), f(i), fbest(i), lu)

        case (None, Some(global)) =>
          X(i) = ngs(X(i), X, m(global), f(i), fbest(i), lu, fl)

        case (None, None) =>
      }

      f(i) = FitnessFunction(X(i))

      if (f(i) > fbest(i)) {
        X(i) = WAS_strategy(X(i), X(rand.nextInt(PopSize)), m(i), f(i), fbest(i), m_GbestPos, D, 10, lu)
        f(i) = FitnessFunction(X(i))
      }

      if (f(i) < fbest(i)) {
        fbest(i) = f(i)
        m(i) = X(i).clone
      }

      if (f(i) < m_GbestVal) {
        m_GbestVal = f(i)
        m_GbestPos = X(i).clone
      }
    }

    Convergence(t) = m_GbestVal
    println(s"Global Fbest: $m_GbestVal")
  }

  private def Fitness(X: Array[Array[Double]]): Array[Double] = X.map(FitnessFunction)

  private def FitnessFunction(x: Array[Double]): Double = matchTest(x, "1")

  private def Generating_Neighborhood(i: Int, X: Array[Array[Double]], mem: Array[Array[Double]], f: Array[Double], fbest: Array[Double]): Array[Array[Double]] = {
    val Out = Array.ofDim[Double](PopSize, D)
    val alpha = 0.02
    val fitnessDiffSum = f.indices.map(j => math.abs(f(i) - fbest(j))).sum

    (0 until PopSize).foreach { k =>
      if (k != i) {
        val W = (alpha + (f(i) - fbest(k))) / fitnessDiffSum
        val distance = math.sqrt((0 until D).map(d => math.pow(X(i)(d) - mem(k)(d), 2)).sum)

        (0 until D).foreach { d =>
          Out(k)(d) = distance * W
        }
      }
    }
    Out
  }

  private def WAS_strategy(x_i: Array[Double], x_r: Array[Double], m_i: Array[Double], f_i_initial: Double, fbest_i_initial: Double, GbestPos: Array[Double], nstep: Int, NJ: Int, lu: Array[Array[Double]]): Array[Double] = {
    val rand = new Random()
    var x_new = x_i.clone
    var f_i = f_i_initial
    var fbest_i = fbest_i_initial

    (0 until NJ).foreach { j =>
      val fl = 2.02 - j * (1.08 / NJ)
      val k = rand.shuffle((0 until x_i.length).toList).take(nstep).toArray

      val Tmp = m_i.clone
      k.foreach { dim =>
        Tmp(dim) = GbestPos(dim) + rand.nextDouble() * fl * (x_r(dim) - x_i(dim))
        Tmp(dim) = math.max(lu(0)(dim), math.min(Tmp(dim), lu(1)(dim)))
      }

      val f_Tmp = FitnessFunction(Tmp)

      if (f_Tmp < f_i) {
        f_i = f_Tmp
        x_new = Tmp.clone
      }

      if (f_Tmp < fbest_i) {
        fbest_i = f_Tmp
        Array.copy(Tmp, 0, m_i, 0, Tmp.length)
      }
    }
    x_new
  }

  private def nls(x_i: Array[Double], m_i: Array[Double], f_i_initial: Double, fbest_i_initial: Double, lu: Array[Array[Double]]): Array[Double] = {
    val rand = new Random()
    var x_new = x_i.clone
    var f_i = f_i_initial
    val nls_steps = 5

    (0 until nls_steps).foreach { _ =>
      val randomDimension = rand.nextInt(D)
      val candidate = x_i.clone
      candidate(randomDimension) = lu(0)(randomDimension) + rand.nextDouble() * (lu(1)(randomDimension) - lu(0)(randomDimension))
      val f_candidate = FitnessFunction(candidate)

      if (f_candidate < f_i) {
        x_new = candidate
        f_i = f_candidate
      }
    }
    x_new
  }

  private def ngs(x_i: Array[Double], X: Array[Array[Double]], GbestPos: Array[Double], f_i_initial: Double, fbest_i_initial: Double, lu: Array[Array[Double]], fl: Double): Array[Double] = {
    val rand = new Random()
    var x_new = x_i.clone
    var f_i = f_i_initial
    val ngs_steps = 5

    (0 until ngs_steps).foreach { _ =>
      val randomCrowIndex = rand.nextInt(PopSize)
      val candidate = x_i.clone

      (0 until D).foreach { d =>
        candidate(d) = x_i(d) + rand.nextDouble() * fl * (GbestPos(d) - X(randomCrowIndex)(d))
        candidate(d) = math.max(lu(0)(d), math.min(candidate(d), lu(1)(d)))
      }

      val f_candidate = FitnessFunction(candidate)

      if (f_candidate < f_i) {
        x_new = candidate
        f_i = f_candidate
      }
    }
    x_new
  }

  private def mean(values: Array[Array[Double]]): Array[Double] = {
    Array.tabulate(D)(j => values.map(_ (j)).sum / values.length)
  }
}

// Main method for testing
object CCSA {
  def main(args: Array[String]): Unit = {
    val D = 25
    val MaxIt = 25000
    val PopSize = 25
    val lu = Array(Array.fill(D)(-100.0), Array.fill(D)(100.0))
    val optimizer = new CCSA(D, MaxIt, PopSize, lu)

    println(optimizer.Convergence.toList)

    println("BEST VALUE")
    println(optimizer.m_GbestVal)
    println(optimizer.m_GbestPos.toList)
  }
}


//import baka.testfunction.matchTest
//import scala.util.Random
//
//class CCSA(val D: Int, val MaxIt: Int, val PopSize: Int, val lu: Array[Array[Double]]) {
//  private val X: Array[Array[Double]] = Array.tabulate(PopSize, D)((_, j) => lu(0)(j) + Random.nextDouble() * (lu(1)(j) - lu(0)(j)))
//  private var m: Array[Array[Double]] = X.map(_.clone)
//  private var f: Array[Double] = computeFitness(X)
//  private var fbest: Array[Double] = f.clone()
//  private var m_GbestPos: Array[Double] = m(getIndexOfMin(f)).clone
//  private var m_GbestVal: Double = fbest.min
//  private var Convergence: Array[Double] = Array.fill(MaxIt)(0.0)
//  Convergence(0) = m_GbestVal
//
//  for (t <- 1 until MaxIt) {
//    val fl = 2.02 - t * (1.08 / MaxIt) // Flight length fl
//
//    for (i <- 0 until PopSize) {
//      val Out = generateNeighborhood(i, X, m, f, fbest).transpose
//      val Mu = Out.transpose.map(_.sum / _.length)
//      val (N, non_N) = computeNeighborhoods(Out, Mu)
//      val Neigh = N.indices.filter(N(_).contains(1))
//      val NonNeigh = non_N.indices.filter(non_N(_).contains(1))
//
//      val Local = Neigh.headOption
//      val Global = NonNeigh.minByOption(fbest)
//
//      (Local, Global) match {
//        case (Some(local), Some(global)) =>
//          X(i) = if (fbest(local) < fbest(global)) nls(X(i), m(local), f(i), fbest(i)) else ngs(X(i), X, m(global), f(i), fbest(i), fl)
//        case (Some(local), None) => X(i) = nls(X(i), m(local), f(i), fbest(i))
//        case (None, Some(global)) => X(i) = ngs(X(i), X, m(global), f(i), fbest(i), fl)
//        case _ => // No action needed if no neighbors are found
//      }
//
//      f(i) = fitnessFunction(X(i))
//
//      if (f(i) < fbest(i)) {
//        X(i) = WAS_strategy(X(i), X(Random.nextInt(PopSize)), m(i), f(i), fbest(i), m_GbestPos, 10)
//        f(i) = fitnessFunction(X(i))
//      }
//
//      if (f(i) < fbest(i)) {
//        fbest(i) = f(i)
//        m(i) = X(i).clone
//      }
//
//      if (f(i) < m_GbestVal) {
//        m_GbestVal = f(i)
//        m_GbestPos = X(i).clone
//      }
//    }
//
//    Convergence(t) = m_GbestVal
//    println(s"Global Fbest: $m_GbestVal")
//  }
//
//  private def computeFitness(X: Array[Array[Double]]): Array[Double] = X.map(fitnessFunction)
//
//  private def fitnessFunction(x: Array[Double]): Double = matchTest(x, "1")
//
//  private def generateNeighborhood(i: Int, X: Array[Array[Double]], m: Array[Array[Double]], f: Array[Double], fbest: Array[Double]): Array[Array[Double]] = {
//    val alpha = 0.02
//    Array.tabulate(PopSize, D) { (k, d) =>
//      if (k != i) {
//        val W = (alpha + (f(i) - fbest(k))) / f.indices.map(j => math.abs(f(i) - fbest(j))).sum
//        val distance = math.sqrt((0 until D).map(dim => math.pow(X(i)(dim) - m(k)(dim), 2)).sum)
//        distance * W
//      } else 0.0
//    }
//  }
//
//  private def computeNeighborhoods(Out: Array[Array[Double]], Mu: Array[Double]): (Array[Array[Int]], Array[Array[Int]]) = {
//    val N = Out.map(row => row.zip(Mu).map { case (x, mu) => if (x < mu) 1 else 0 })
//    val non_N = Out.map(row => row.zip(Mu).map { case (x, mu) => if (x > mu) 1 else 0 })
//    (N, non_N)
//  }
//
//  private def WAS_strategy(x_i: Array[Double], x_r: Array[Double], m_i: Array[Double], f_i: Double, fbest_i: Double, GbestPos: Array[Double], NJ: Int): Array[Double] = {
//    var x_new = x_i.clone
//    var current_f = f_i
//    var current_fbest = fbest_i
//
//    for (j <- 0 until NJ) {
//      val fl = 2.02 - j * (1.08 / NJ)
//      val k = Random.shuffle((0 until D).toList).take(10)
//
//      val Tmp = m_i.clone
//      for (dim <- k) {
//        Tmp(dim) = GbestPos(dim) + Random.nextDouble() * fl * (x_r(dim) - x_i(dim))
//        Tmp(dim) = Tmp(dim).max(lu(0)(dim)).min(lu(1)(dim))
//      }
//
//      val f_Tmp = fitnessFunction(Tmp)
//      if (f_Tmp < current_f) {
//        current_f = f_Tmp
//        x_new = Tmp.clone
//      }
//
//      if (f_Tmp < current_fbest) {
//        current_fbest = f_Tmp
//        Array.copy(Tmp, 0, m_i, 0, Tmp.length)
//      }
//    }
//    x_new
//  }
//
//  private def nls(x_i: Array[Double], m_i: Array[Double], f_i: Double, fbest_i: Double): Array[Double] = {
//    var x_new = x_i.clone
//    var current_f = f_i
//
//    for (_ <- 0 until 5) {
//      val candidate = x_i.clone
//      val randomDimension = Random.nextInt(D)
//      candidate(randomDimension) = lu(0)(randomDimension) + Random.nextDouble() * (lu(1)(randomDimension) - lu(0)(randomDimension))
//
//      val f_candidate = fitnessFunction(candidate)
//      if (f_candidate < current_f) {
//        x_new = candidate
//        current_f = f_candidate
//      }
//    }
//    x_new
//  }
//
//  private def ngs(x_i: Array[Double], X: Array[Array[Double]], GbestPos: Array[Double], f_i: Double, fbest_i: Double, fl: Double): Array[Double] = {
//    var x_new = x_i.clone
//    var current_f = f_i
//
//    for (_ <- 0 until 5) {
//      val randomCrowIndex = Random.nextInt(PopSize)
//      val candidate = x_i.clone
//
//      for (d <- 0 until D) {
//        candidate(d) = x_i(d) + Random.nextDouble() * fl * (GbestPos(d) - X(randomCrowIndex)(d))
//        candidate(d) = candidate(d).max(lu(0)(d)).min(lu(1)(d))
//      }
//
//      val f_candidate = fitnessFunction(candidate)
//      if (f_candidate < current_f) {
//        x_new = candidate
//        current_f = f_candidate
//      }
//    }
//    x_new
//  }
//
//  private def getIndexOfMin(array: Array[Double]): Int = array.indices.minBy(array)
//
//}
//
//// Main method for testing
//object CCSA {
//  def main(args: Array[String]): Unit = {
//    val D = 25 // Example: 25 dimensions
//    val MaxIt = 25000 // Maximum iterations
//    val PopSize = 25 // Population size
//    val lu = Array.fill(2, D)(0.0)
//
//    for (i <- 0 until D) {
//      lu(0)(i) = -100.0
//      lu(1)(i) = 100.0
//    }
//
//    val ccsa = new CCSA(D, MaxIt, PopSize, lu)
//    println("Done")
//  }
//}
//
