


package com.dust.SCSA

import java.io._
import java.nio.file.{Files, Paths}


object npver1 extends Serializable {


  def saveFile(arr: Array[Array[Double]]): Unit = {
    val Path = "/home/nerdypants/Desktop/XSCSA.txt"
    Files.deleteIfExists(Paths.get(Path))
    val fw2 = new FileWriter(Path)
    for (i <- 0 until arr.length) {
      for (j <- 0 until arr.length) {
        if (j != (arr.length - 1)) {
          fw2.write(arr(i)(j) + ",")
        }
        else {
          fw2.write(arr(i)(j) + "")
        }
      }
      fw2.write("\n")
    }
    fw2.close()
  }

  //taking mean of ' out ' for generating neighborhood
  def mean(arr: List[Double]): Double = {
    return (arr.sum / arr.length).toFloat
  }


  def sccsa(migration_interval: Int, pop: Int, dim: Int, min: Double, max: Double, max_it: Int, selection: Int, casef: String) = {

    val conf = new SparkConf().setAppName("SCCSA").setMaster("local[*]")
    val sc = new SparkContext(conf)
    val numSlices = 4

    //i=traversing through pop of crows
    //var i:Int=0
    //t = current iteration 
    var t = 0

    //global best value of the crow
    var best: Double = 0.0
    // to store the results of all iterations
    var convergence = new Array[Double](max_it)
    var converge_address = Array.ofDim[Double](max_it, dim)


    //Initializing the List of List for crows 
    var x = List.fill(pop)(List.fill(dim)((min + scala.util.Random.nextDouble() * (max - min))).toArray).toArray
    println("Intilaization space : ")
    println(x)


    //mapping fitnesses to place it in objects
    var fitnessesC = x.map(com.dust.SCSA.testfunction.matchTest(_, casef))
    //maping fitnesses of all crows
    println("Fitness", fitnessesC.toList)


    println("population")
    // creating instances of objects
    var population = (x, List.range(0, pop)).zipped.map((x_p, i) => new com.dust.SCSA.Crow1(x_p, pop, dim, min, max, fitnessesC(i)))
    println("Population ", population)


    var sortedpopulation = population.sortWith(_.fbest < _.fbest).clone()
    val broadcastVar = BroadcastWrapper(sc, sortedpopulation.slice(0, (((selection * pop) / 100) / numSlices)))
    var topcrow: Array[Array[Double]] = Array.ofDim[Double](2, dim)
    topcrow(0) = sortedpopulation(0).y.clone()
    topcrow(1) = sortedpopulation(1).y.clone()
    val top2crow = BroadcastWrapper(sc, topcrow)


    var para = sc.parallelize(population, numSlices).persist()

    var fl = BroadcastWrapper(sc, population(0).flight(max_it, t))
    var mi = BroadcastWrapper(sc, migration_interval)

    while (t < max_it) {

      //stores the flight length
      fl.update(population(0).flight(max_it, t))
      println("flight length ", fl.value)


      val RDD = para.mapPartitionsWithIndex {
        (idx, iterator) =>
          //println("Partition ",idx)
          var crows = iterator.toArray
          var bestsol = broadcastVar.value
          crows = crows ++ bestsol
          /*for(i<-0 until bestsol.length){
            crows(i)=bestsol(i)
          }*/
          var Fl = fl.value
          var top2 = top2crow.value
          var sortcrows = crows.sortWith(_.fbest < _.fbest)
          var i = 0
          val partitionsize = crows.length
          //Main loop for traversing through population
          var it = 0
          val mii = mi.value
          while (it < mii) {


            while (i < partitionsize) {


              // for finding the "out" for crow(i) with respect to all other crows
              var out = List.range(0, crows.length).map((j) => crows(i).Neighbor(crows(i).x, crows(j).y))
              //the Mu =mean of "out"
              var mu: Double = mean(out)

              //creating Neigh and Non-Neigh
              var neigh = new Array[Int](crows.length)
              var N: Int = 0
              var non_Neigh = new Array[Int](crows.length)
              var Non_N: Int = 0

              for (z <- 0 until crows.length) {

                if (out(z) < mu) {
                  neigh(N) = z
                  N += 1
                } else if (out(z) >= mu) {
                  non_Neigh(Non_N) = z
                  Non_N += 1
                }
              }
              //slicing the length of the arrays
              neigh = neigh.slice(0, N)
              non_Neigh = non_Neigh.slice(0, Non_N)
              //Now we have list of Neigh and Non_Neigh contain9ing the indexes of the crows


              var randomindex: Int = 0
              var local = 0
              //selecting random local crow
              try {
                randomindex = scala.util.Random.nextInt(neigh.length)
                local = neigh(randomindex)
              }
              catch {
                case e: java.lang.IllegalArgumentException => {
                  local = 0
                  println("error the local length is : ", randomindex) //," ",neigh(randomindex))
                }
              }
              //selecting global best crow from Non-Neigh
              try {
                if (non_Neigh.length > 0)
                  non_Neigh = non_Neigh.sortWith(crows(_).fbest < crows(_).fbest).clone()

              }
              catch {
                case e: java.lang.IllegalArgumentException => println("Comparison error , the length is : ", non_Neigh.length)
              }
              var global: Int = non_Neigh(0)


              //Deciding Strategy
              if (crows(local).fbest < crows(global).fbest) {
                //call NLS
                crows(i).x = crows(i).NLS(crows(i).x, crows(local).y, Fl)
              }
              else {
                //call NGS
                crows(i).x = crows(i).NGS(crows(i).x, crows(global).y, Fl)
              }

              //to check and correct the crow
              crows(i).x = crows(i).CC(crows(i).x)

              //calling fitness function
              crows(i).f = com.dust.SCSA.testfunction.matchTest(crows(i).x, casef)


              /*//old strategy... but you can uncomment it and comment the PRC to check the difference in result
              var best2=top2crow.value
              //Call WAS to make the crow wander
              if (crows(i).fbest<crows(i).fitnesses){
              //deciding random jumps between 50
                var randjump =scala.util.Random.nextInt(50)+1
                //full flight length
                var ffl=crows(0).flight(max_it, 1)
                // this for random number of jumps
                for(j<-0 until randjump){
                //to get index for random crow from the population
                  val x_r=scala.util.Random.nextInt(crows.length)
                  //calling the WAS strategy
                  crows(i).x=crows(i).WAS(crows(i).x,crows(x_r).x,best2(0),ffl).toArray
                }
                val resprc=com.dust.SCSA.testfunction.matchTest( crows(i).x,casef)
                  crows(i).f=resprc

              }*/


              // Calling PRC
              //creating probability factor similar to the one in simulated anealing
              val probability = math.exp((-(math.abs(crows(i).fbest - crows(i).f))) / Fl)
              //condition for PRC
              if ((probability > scala.util.Random.nextDouble()) && crows(i).f > crows(i).fbest) {
                //get top crows
                //var top2=top2crow.value
                // generating random crow in between the two crows
                val randcrow = crows(i).prc(top2(0), top2(1))
                val resprc = com.dust.SCSA.testfunction.matchTest(randcrow, casef)
                crows(i).x = randcrow.clone()
                crows(i).f = resprc


              }


              //Definition 1 updating memory
              if (crows(i).f < crows(i).fbest) {
                crows(i).y = crows(i).x.clone()
                crows(i).fbest = crows(i).f
              }

              i += 1

            }
            try {
              sortcrows = crows.sortWith(_.fbest < _.fbest).clone()
            }
            catch {
              case e: java.lang.IllegalArgumentException => println("Error in comparison , the length is : ", sortcrows.length)
            }

            val tmptop = com.dust.SCSA.testfunction.matchTest(top2(0), casef)
            if (sortcrows(0).fbest < tmptop) {
              top2(0) = sortcrows(0).y.clone()
              top2(1) = sortcrows(1).y.clone()
            }
            it += 1
            i = 0
          }
          t = 0


          val select = ((selection * crows.length) / 100)

          /* //replace the population at random
           for(i<-0 until selection){
             crows(0 + scala.util.Random.nextInt()*(selection - 0))=sortedpopulation(i)
           }*/

          var selected = sortcrows.slice(0, select)
          //var selected=crows.slice(0, select)
          //println("select=",select)
          //broadcastVar.update(selected.clone())
          //brdtest.update(selected)
          // println("done")
          selected.toIterator
      }.cache()

      sortedpopulation = RDD.collect()
      RDD.unpersist()
      sortedpopulation = sortedpopulation.sortWith(_.fbest < _.fbest).clone()
      broadcastVar.update(sortedpopulation.slice(0, (sortedpopulation.length / (numSlices - 1))))
      topcrow(0) = sortedpopulation(0).y.clone()
      topcrow(1) = sortedpopulation(1).y.clone()
      top2crow.update(topcrow)


      convergence(t) = sortedpopulation(0).fbest
      converge_address(t) = sortedpopulation(0).y
      println("fbest at t=", t, "is val=", convergence(t))


      t = t + migration_interval
    }



    //Printing the overall best values and the convergence list and the crow dimension values !
    println("\n\nThe Convergence ", convergence.toList)

    println("The best value out of convergence is ", convergence.toList.reduce((x, y) => x min y))

    println("The of the overall best crow is :", converge_address(convergence.indexOf(convergence.toList.reduce((x, y) => x min y))).toList, "\tGlobal best", convergence.toList.reduce((x, y) => x min y))


  }


}  





