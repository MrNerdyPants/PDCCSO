package serialCrow

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
  private val X: Array[Array[Double]] = Array.ofDim[Double](PopSize, D) // Positions of crows
  private var m: Array[Array[Double]] = Array.ofDim[Double](PopSize, D) // Hiding places of crows
  private var f: Array[Double] = Array.ofDim[Double](PopSize) // Fitness values
  private var fbest: Array[Double] = Array.ofDim[Double](PopSize) // Best fitness values for each crow
  private var m_GbestPos: Array[Double] = Array.ofDim[Double](D) // Global best position
  private var m_GbestVal: Double = 0.0 // Global best fitness value
  private var Convergence: Array[Double] = Array.ofDim[Double](MaxIt) // Array to store convergence over iterations

  // Initialize population
  private val rand = new Random()
  for (i <- 0 until PopSize) {
    for (j <- 0 until D) {
      X(i)(j) = lu(0)(j) + rand.nextDouble() * (lu(1)(j) - lu(0)(j))
    }
  }

  // Set the hiding place of each crow and evaluate fitness
  m = X.map(_.clone)
  f = Fitness(X)
  fbest = f.clone()
  m_GbestVal = findMin(f)
  m_GbestPos = m(getIndexOfMin(f)).clone
  Convergence(0) = m_GbestVal

  // Main optimization loop
  for (t <- 1 until MaxIt) {
    val fl = 2.02 - t * (1.08 / MaxIt) // Flight length fl

    for (i <- 0 until PopSize) {
      // Conscious Neighborhood
      // Generate neighborhood using existing method
      val Out = Generating_Neighborhood(i, X, m, f, fbest).transpose
      val Mu = mean(Out) // Mean calculation

      // Determine the neighborhood N (ci) and non-neighborhood for crow ci based on Definition 2
      val N = Out.map(row => row.zip(Mu).map { case (x, mu) => if (x < mu) 1 else 0 })
      val non_N = Out.map(row => row.zip(Mu).map { case (x, mu) => if (x > mu) 1 else 0 })


      // Determine the neighborhood N (ci) and non-neighborhood for crow ci
      val Neigh = N.zipWithIndex.collect {
        case (row, rowIndex) if row.contains(1) =>
          rowIndex // Collect the row index if it contains a neighbor
      }
      // The non-neighborhood for crow ci
      val Nun_Neigh = non_N.zipWithIndex.collect {
        case (row, rowIndex) if row.contains(1) =>
          rowIndex // Collect the row index if it contains a non-neighbor
      }


      // Determine a random neighbor (c_local) and the best non-neighbor (c_global) of ci
      val Local: Option[Int] = if (Neigh.nonEmpty) {
        Some(Neigh(scala.util.Random.nextInt(Neigh.size)))
      } else {
        None // If empty, set to None
      }

      val Global: Option[Int] = if (Nun_Neigh.nonEmpty) {
        Some(Nun_Neigh.minBy(idx => fbest(idx)))
      } else {
        None // If empty, set to None
      }

      // Use pattern matching to handle the value of Local and Global
      (Local, Global) match {
        case (Some(local), Some(global)) =>
          if (fbest(local) < fbest(global)) {
            // Call NLS strategy
            X(i) = nls(X(i), m(local), f(i), fbest(i), lu)
          } else {
            // Call NGS strategy
            X(i) = ngs(X(i), X, m(global), f(i), fbest(i), lu, fl)
          }

        case (Some(local), None) =>
          // If Global is None, use the NLS strategy by default
          X(i) = nls(X(i), m(local), f(i), fbest(i), lu)

        case (None, Some(global)) =>
          // If Local is None, use the NGS strategy by default
          X(i) = ngs(X(i), X, m(global), f(i), fbest(i), lu, fl)

        case (None, None) =>
        // Handle the case where both Local and Global are None (no neighbors found)
        // You might want to log a warning or take some default action
      }

      f(i) = FitnessFunction(X(i))

      if (f(i) > fbest(i)) {
        // Perform wandering strategy for search
        X(i) = WAS_strategy(X(i), X(rand.nextInt(PopSize)), m(i), f(i), fbest(i), m_GbestPos, D, 10, lu)
        f(i) = FitnessFunction(X(i))
      }



      // Update positions and fitness values
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
    println("Global Fbest: ", m_GbestVal)
  }

  // Method to calculate fitness
  private def Fitness(X: Array[Array[Double]]): Array[Double] = {
    X.map(FitnessFunction)
  }

  private def FitnessFunction(x: Array[Double]): Double = {
    // Example: Sphere function
    //    x.map(xi => xi * xi).sum
    matchTest(x, "1")
  }

  // Method to generate the conscious neighborhood of a crow
  private def Generating_Neighborhood(i: Int, X: Array[Array[Double]], mem: Array[Array[Double]], f: Array[Double], fbest: Array[Double]): Array[Array[Double]] = {
    val Out = Array.ofDim[Double](PopSize, D)
    val alpha = 0.02
    for (k <- 0 until PopSize if k != i) {
      val fitnessDiffSum = f.indices.map(j => math.abs(f(i) - fbest(j))).sum
      val W = (alpha + (f(i) - fbest(k))) / fitnessDiffSum

      // Compute Euclidean distance between X[i] and mem[k]
      val distance = math.sqrt((0 until D).map(d => math.pow(X(i)(d) - mem(k)(d), 2)).sum)

      for (d <- 0 until D) {
        Out(k)(d) = distance * W
      }
    }
    Out
  }

  private def WAS_strategy(x_i: Array[Double], x_r: Array[Double], m_i: Array[Double], f_i_initial: Double, fbest_i_initial: Double, GbestPos: Array[Double], nstep: Int, NJ: Int, lu: Array[Array[Double]]): Array[Double] = {
    val rand = new Random()
    var x_new = x_i.clone
    var f_i = f_i_initial // Make f_i mutable
    var fbest_i = fbest_i_initial // Make fbest_i mutable

    for (j <- 0 until NJ) {
      val fl = 2.02 - j * (1.08 / NJ)
      val k = rand.shuffle((0 until x_i.length).toList).take(nstep).toArray

      val Tmp = m_i.clone
      for (dim <- k) {
        Tmp(dim) = GbestPos(dim) + rand.nextDouble() * fl * (x_r(dim) - x_i(dim))
        Tmp(dim) = math.max(lu(0)(dim), math.min(Tmp(dim), lu(1)(dim))) // Keep within bounds
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
    var f_i = f_i_initial // Make f_i mutable
    val nls_steps = 5 // Number of local search steps

    for (step <- 0 until nls_steps) {
      val randomDimension = rand.nextInt(D)
      val candidate = x_i.clone

      // Mutate in the selected dimension
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
    var f_i = f_i_initial // Make f_i mutable
    val ngs_steps = 5 // Number of global search steps

    for (step <- 0 until ngs_steps) {
      val randomCrowIndex = rand.nextInt(PopSize)
      val candidate = x_i.clone

      for (d <- 0 until D) {
        candidate(d) = x_i(d) + rand.nextDouble() * fl * (GbestPos(d) - X(randomCrowIndex)(d))
        candidate(d) = math.max(lu(0)(d), math.min(candidate(d), lu(1)(d))) // Keep within bounds
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
    val meanValues = new Array[Double](D)
    for (j <- 0 until D) {
      meanValues(j) = values.map(_ (j)).sum / values.length
    }
    meanValues
  }

  private def findMin(array: Array[Double]): Double = {
    array.min
  }

  private def getIndexOfMin(array: Array[Double]): Int = {
    array.indices.minBy(array)
  }
}

// Main method for testing
object CCSA {
  def main(args: Array[String]): Unit = {
    val D = 25 // Example: 10 dimensions
    val MaxIt = 25000 // Maximum iterations
    val PopSize = 25 // Population size
    val lu = Array.fill(2, D)(0.0)

    // Define bounds for each dimension
    for (i <- 0 until D) {
      lu(0)(i) = -100.0 // Lower bound
      lu(1)(i) = 100.0 // Upper bound
    }

    val ccsa = new CCSA(D, MaxIt, PopSize, lu)
    println("Done")
  }
}

