
package baka

import org.apache.commons.math3.util.FastMath.floor
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import java.io._
import scala.collection.mutable.{ArrayBuffer, HashMap}


object npver5 extends Serializable {


  //taking mean of ' out ' for generating neighborhood
  def mean(arr: List[Double]): Double = {
    return (arr.sum / arr.length).toFloat
  }


  def sccsa(mi: Int, Pop: Int, dim: Int, min: Double, max: Double, max_it: Int, selection: Int, casef: String) = {

    val conf = new SparkConf().setAppName("SCCSA").setMaster("local[*]")
    val sc = new SparkContext(conf)
    val numSlices = 2


    //i=traversing through pop of crows
    //var i:Int=0
    //t = current iteration 
    var t = 0

    //global best value of the crow
    var best: Double = 0.0
    // to store the results of all iterations
    var convergence = new Array[Double]((max_it / mi) + mi)
    var converge_address = new Array[Double](dim) //Array.ofDim[Double](max_it,dim)
    var con_t = 0


    //Initializing the List of List for crows 
    var x = List.fill(Pop)(List.fill(dim)((min + scala.util.Random.nextDouble() * (max - min))).toArray).toArray
    println("Intilaization space : ")
    println(x)


    //mapping fitnesses to place it in objects
    var fitnessesC = x.map(baka.testfunction.matchTest(_, casef))
    //maping fitnesses of all crows
    println("Fitness", fitnessesC.toList)


    println("population")
    // creating instances of objects
    var population = (x, List.range(0, Pop)).zipped.map((x_p, i) => new baka.Crow1(x_p, Pop, dim, min, max, fitnessesC(i))).toArray
    println("Population ", population, "\n|Population length : ", population.length)

    val sel = BroadcastWrapper(sc, selection)
    var sortedpopulation = population.sortWith(_.fBest < _.fBest).clone()
    val perc = ((((population.length).toDouble / (100).toDouble).toDouble * selection) / numSlices).toInt
    //val broadcastVar = BroadcastWrapper(  sc,  sortedpopulation.slice(0, (((newperc*Pop).ceil.toInt)))  )
    val broadcastVar = BroadcastWrapper(sc, sortedpopulation.take(perc))
    println("Initial  brd pop length ", (broadcastVar.value).length)
    var topcrow: Array[Array[Double]] = Array.ofDim[Double](2, dim)
    topcrow(0) = sortedpopulation(0).y.clone()
    topcrow(1) = sortedpopulation(1).y.clone()
    val top2crow = BroadcastWrapper(sc, topcrow)

    val mii = BroadcastWrapper(sc, mi)

    var para: RDD[Crow1] = sc.parallelize(population, numSlices).persist()

    var fl = BroadcastWrapper(sc, population(0).flight(max_it, t))
    val hashMap = HashMap[String, Int]()
    hashMap += ("t" -> t)
    hashMap += ("max_it" -> max_it)
    var iter = BroadcastWrapper(sc, hashMap)

    var cflag = BroadcastWrapper(sc, false)
    var delta1: Double = 0
    var delta2: Double = 0
    var l1: Double = 0
    var l2: Double = 0
    val micount = 5
    var mc = 0
    while (t < max_it) {

      l1 = sortedpopulation(0).fBest

      //stores the flight length
      fl.update(population(0).flight(max_it, t))
      println("flight length ", fl.value)

      hashMap += ("t" -> t)
      hashMap += ("max_it" -> max_it)
      iter.update(hashMap)


      val RDD = para.mapPartitionsWithIndex {
        (idx, iterator) =>
          //variable iniatlizations
          //          print("partition :" + idx)


          //println("Partition ",idx)
          var crows = iterator.toArray
          var bestsol = broadcastVar.value
          crows = crows ++ bestsol

          val inter_Iter = iter.value
          var Fl = fl.value
          var node_t = 0
          val node_mi = mii.value
          var top2 = top2crow.value
          var topchk = crows.sortWith(_.fBest < _.fBest) //.take(2)
          var i = 0
          val partitionsize = crows.length
          while (node_t < node_mi) {

            Fl = crows(0).flight(inter_Iter("max_it"), inter_Iter("t") + node_t)

            //Main loop for traversing through population
            while (i < partitionsize) {


              // for finding the "out" for crow(i) with respect to all other crows
              var i_kDiff = (crows(i).sumf(crows(i).f, crows.map(_.fBest).toArray, 0, crows.length)) //diff=crows(i).sumf(crows(i).f,crows.map(_.fbest).toArray ,0,crows.length)


              val out: Array[Double] = (0 until crows.length)
                .filter(_ != i) // Exclude the index `i`
                .map { j =>
                  crows(i).neighbor(crows(i).x, crows(j).y, crows(i).f, crows(j).fBest, i_kDiff)
                }.toArray


              //the Mu =mean of "out"
              var mu: Double = mean(out.toList)

              //              //creating Neigh and Non-Neigh

              val neighbours: ArrayBuffer[Crow1] = ArrayBuffer[Crow1]();
              var non_Neighbours: ArrayBuffer[Crow1] = ArrayBuffer[Crow1]();

              // Iterate over all crows to classify them into neighbors and non-neighbors
              for (z <- 0 until crows.length - 1) {
                if (out(z) < mu) {
                  //                  neighBuffer += z
                  neighbours += crows(z)
                } else if (out(z) >= mu) {
                  //                  nonNeighBuffer += z
                  non_Neighbours += crows(z)
                }
              }


              //selecting random local crow
              var randomindex: Int = scala.util.Random.nextInt(neighbours.length)
              var local: Crow1 = neighbours(randomindex)



              //selecting global best crow from Non-Neigh
              non_Neighbours = non_Neighbours.sortWith(_.fBest < _.fBest).clone()

              var global: Crow1 = non_Neighbours(0)



              //Deciding Strategy
              if (local.fBest < global.fBest) {
                //call NLS
                crows(i).x = crows(i).NLS(crows(i).x, local.y, Fl)
              }
              else {
                //call NGS
                crows(i).x = crows(i).NGS(crows(i).x, global.y, Fl)
              }

              //to check and correct the crow
              crows(i).x = crows(i).CC(crows(i).x)

              //calling fitness function
              crows(i).f = baka.testfunction.matchTest(crows(i).x, casef)


              //old strategy... but you can uncomment it and comment the PRC to check the difference in result
              //Call WAS to make the crow wander
              if (crows(i).fBest < crows(i).fitnesses) {
                //deciding random jumps between 50
                var randjump = floor((scala.util.Random.nextDouble() * dim)).toInt + 1
                //full flight length
                //                var ffl = crows(0).flight(max_it, 1)
                // this for random number of jumps
                for (j <- 0 until randjump) {
                  //to get index for random crow from the population
                  val x_r = scala.util.Random.nextInt(crows.length)
                  //calling the WAS strategy
                  crows(i).x = crows(i).WAS(crows(i).x, crows(x_r).x, top2(0), Fl).toArray
                }
                val resprc = baka.testfunction.matchTest(crows(i).x, casef)
                crows(i).f = resprc

              }



              //              // Calling PRC
              //              //creating probability factor similar to the one in simulated anealing
              //              val probability = math.exp((-(math.abs(crows(i).fBest - crows(i).f))) / Fl)
              //              //condition for PRC
              //              if ((probability > scala.util.Random.nextDouble()) && crows(i).f > crows(i).fBest) {
              //                //get top crows
              //                //top2=top2crow.value
              //                // generating random crow in between the two crows
              //                val randcrow = crows(i).prc(top2(0), top2(1))
              //                val resprc = baka.testfunction.matchTest(randcrow, casef)
              //                crows(i).x = randcrow.clone()
              //                crows(i).f = resprc
              //
              //
              //              }


              //Definition 1 updating memory
              if (crows(i).f < crows(i).fBest) {
                crows(i).y = crows(i).x.clone()
                crows(i).fBest = crows(i).f
              }

              i += 1

            }
            i = 0
            node_t += 1
            topchk = crows.sortWith(_.fBest < _.fBest).clone()
            val tmpres = baka.testfunction.matchTest(top2(0), casef)
            if (topchk(0).fBest < tmpres) {
              top2(0) = topchk(0).y
              top2(1) = topchk(1).y
            }


          }
          node_t = 0


          var selected = /*crows.sortWith(_.fbest<_.fbest)*/ topchk.take((((sel.value.toDouble * (crows.length).toDouble / (100).toDouble))).toInt) //.take((crows.length/100)*20)

          selected.toIterator
      }.persist()

      sortedpopulation = RDD.collect()
      //RDD.unpersist()
      sortedpopulation = sortedpopulation.sortWith(_.fBest < _.fBest).clone()


      l2 = sortedpopulation(0).fBest
      var delta1 = 1 + (l2 - l1).abs
      if (t == mi) {
        delta2 = delta1 - 1
      }
      if (delta1 <= delta2) {
        mc += 1
      } else {
        mc = 0
      }
      if (mc == micount) {
        cflag.update(true)
        println("\n\npop change\n\n")
        mc = 0
      }
      delta2 = delta1

      println("population size before slice ", sortedpopulation.length)
      //sortedpopulation=sortedpopulation.slice(0, (sortedpopulation.length/(numSlices))).clone
      val sortedpopulation1 = sortedpopulation.take((perc) + 1).clone()

      println("size after slice ", sortedpopulation.length)
      broadcastVar.update(sortedpopulation1)
      topcrow(0) = sortedpopulation(0).y.clone()
      topcrow(1) = sortedpopulation(1).y.clone()
      top2crow.update(topcrow)


      convergence(con_t) = sortedpopulation(0).fBest
      converge_address = sortedpopulation(0).y.clone()

      println("fbest at t=", t + mi, "is val=", convergence(con_t))
      con_t += 1



      //t=t+1
      t += mi
    }



    //Printing the overall best values and the convergence list and the crow dimension values !
    println("\n\nThe Convergence ", convergence.toList)

    println("The best value out of convergence is ", convergence(con_t - 1))
    //println("The best value out of convergence is ",convergence.toList.reduce((x,y)=> x min y))

    println("The best value out of convergence is ", converge_address.toList)
    //println("The of the overall best crow is :",converge_address(convergence.indexOf(convergence.toList.reduce((x,y)=> x min y))).toList,"\tGlobal best",convergence.toList.reduce((x,y)=> x min y))


  }


}  






