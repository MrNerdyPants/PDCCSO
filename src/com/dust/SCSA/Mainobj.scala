package com.dust.SCSA

import scala.util.control.Breaks._
import org.apache.log4j._
import org.apache.spark._
import org.apache.spark.sql.SparkSession
import org.apache.spark.mllib.rdd.RDDFunctions._
import scala.io.Source
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.stats.distributions._
import org.apache.spark.rdd.RDD


object Mainobj {
  
  
/*
 * This is the main function where the objects and their functions will be called
 */

  def main(args:Array[String]){  
     // Set the log level to only print errors
    Logger.getLogger("org").setLevel(Level.ERROR)
     val t1 = System.nanoTime
    
    //Data Members for initialization\
    //getting inputs
    println("Enter the Population:")
    val pop : Int = 50//scala.io.StdIn.readLine.toInt//args(0).toInt//scala.io.StdIn.readLine.toInt//
    
    println("Enter the Dimensions:")
    val dim : Int = 100//scala.io.StdIn.readLine.toInt//args(1).toInt//scala.io.StdIn.readLine.toInt//
    
    println("Enter the Min Range:")
    val min : Double = -100//scala.io.StdIn.readLine.toDouble//args(2).toDouble//scala.io.StdIn.readLine.toDouble//
    
    println("Enter the Max Range:")
    val max : Double = 100//scala.io.StdIn.readLine.toDouble//args(3).toDouble//scala.io.StdIn.readLine.toDouble//
    
    println("Enter the Max Iterations:")
    val max_it : Int = 2000000//scala.io.StdIn.readLine.toInt//args(4).toInt//scala.io.StdIn.readLine.toInt//
    
    println("What should be the percentage of population selection(25,50,75):")
    var selection : Int = 10//scala.io.StdIn.readLine.toInt//args(5).toInt//scala.io.StdIn.readLine.toInt//
   // selection=(selection/100)*pop
    
    println("Enter the Function case :")
    val casef : String = "1"//scala.io.StdIn.readLine.toString//args(6).toString//scala.io.StdIn.readLine.toString//
     
    println("Enter the migration interval :")
    val mi : Int = 2000//scala.io.StdIn.readLine.toInt//args(7).toString//scala.io.StdIn.readLine.toString//
     

    com.dust.SCSA.npver5.sccsa(mi, pop ,dim ,min ,max ,max_it ,selection ,casef )
    
        // As it is a nano second we need to divide it by 1000000000. in 1e9d "d" stands for double
    val duration = (System.nanoTime - t1) / 1e9d
    
    println("Timer", duration)
    
   
  }
  

 

}  
