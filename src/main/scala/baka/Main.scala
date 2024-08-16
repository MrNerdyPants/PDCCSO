package baka

import java.util.logging.{Level, Logger}

/**
 *
 * @project CCN
 * @file Main
 * @author Kashan Asim on 13/08/2024.
 *
 */
object Main {

  def main(args: Array[String]): Unit = {
    // Set the log level to only print errors
    Logger.getLogger("org").setLevel(Level.OFF)
    Logger.getLogger("akka").setLevel(Level.OFF)
    //    Logger.getLogger("org").setLevel(Level.SEVERE)
    val t1 = System.nanoTime

    //Data Members for initialization\
    //getting inputs
    println("Enter the Population:")
    val pop: Int = 25 //scala.io.StdIn.readLine.toInt//args(0).toInt//scala.io.StdIn.readLine.toInt//

    println("Enter the Dimensions:")
    val dim: Int = 25 //scala.io.StdIn.readLine.toInt//args(1).toInt//scala.io.StdIn.readLine.toInt//

    println("Enter the Min Range:")
    val min: Double = -5.12 //scala.io.StdIn.readLine.toDouble//args(2).toDouble//scala.io.StdIn.readLine.toDouble//

    println("Enter the Max Range:")
    val max: Double = 5.12 //scala.io.StdIn.readLine.toDouble//args(3).toDouble//scala.io.StdIn.readLine.toDouble//

    println("Enter the Max Iterations:")
    val max_it: Int = 25000 //scala.io.StdIn.readLine.toInt//args(4).toInt//scala.io.StdIn.readLine.toInt//

    println("What should be the percentage of population selection(25,50,75):")
    var selection: Int = 70 //scala.io.StdIn.readLine.toInt//args(5).toInt//scala.io.StdIn.readLine.toInt//
    // selection=(selection/100)*pop

    println("Enter the Function case :")
    val casef: String = "2" //scala.io.StdIn.readLine.toString//args(6).toString//scala.io.StdIn.readLine.toString//

    println("Enter the migration interval :")
    val mi: Int = 1000 //scala.io.StdIn.readLine.toInt//args(7).toString//scala.io.StdIn.readLine.toString//


    baka.npver5.sccsa(mi, pop, dim, min, max, max_it, selection, casef)

    // As it is a nano second we need to divide it by 1000000000. in 1e9d "d" stands for double
    val duration = (System.nanoTime - t1) / 1e9d

    println("Timer", duration)
  }

}
