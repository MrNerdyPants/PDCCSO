


package com.dust.SCSA

import scala.util.control.Breaks._
import java.io._
import scala.io.Source
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd
import org.apache.spark._
import org.apache.spark.rdd.RDD
import scala.util.Random
import scala.util.control.Breaks._
import java.util.Calendar
import java.io.FileWriter
import java.nio.file.Files
import java.nio.file.Paths



import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import org.apache.spark.serializer.Serializer
import org.apache.hadoop.io.serializer.Serializer
import shapeless._0
import org.apache.spark.broadcast
import org.apache.spark.broadcast.Broadcast
import shapeless.Lens
import shapeless.ops.tuple.Length
//import com.dust.SCSA.BroadcastWrapper





object npver2 extends Serializable {
  

//taking mean of ' out ' for generating neighborhood
def mean(arr:List[Double]):Double={    
    return (arr.sum/arr.length).toFloat
}



    

    
    def sccsa(mi : Int, Pop : Int,dim : Int,min : Double,max : Double,max_it : Int,selection : Int,casef : String)={ 
      
     val conf = new SparkConf().setAppName("SCCSA").setMaster("local[*]")
     val sc = new SparkContext(conf)
      val numSlices=4
      

    
      //i=traversing through pop of crows
    //var i:Int=0
    //t = current iteration 
    var t=0
    
    //global best value of the crow
    var best:Double=0.0
    // to store the results of all iterations
    var convergence=new Array[Double]((max_it/mi)+mi)
    var converge_address= new Array[Double](dim)//Array.ofDim[Double](max_it,dim)
    var con_t=0

     
    //Initializing the List of List for crows 
    var x = List.fill(Pop)(List.fill(dim)((min + scala.util.Random.nextDouble()*(max - min))).toArray).toArray
    println("Intilaization space : ")
    println(x)
   
    
   //mapping fitnesses to place it in objects 
   var fitnessesC=x.map(com.dust.SCSA.testfunction.matchTest(_, casef))
   //maping fitnesses of all crows 
    println("Fitness",fitnessesC.toList) 
   

   
    println("population")
    // creating instances of objects
    var population=(x,List.range(0,Pop)).zipped.map((x_p,i)=>new com.dust.SCSA.Crow1(x_p, Pop, dim, min, max,fitnessesC(i)))
    println("Population ",population ,"\n|Population length : ",population.length)
    
    
    var sortedpopulation=population.sortWith(_.fbest<_.fbest).clone()
    val perc=((((population.length).toDouble/(100).toDouble).toDouble*selection)/numSlices).toInt
    //val broadcastVar = BroadcastWrapper(  sc,  sortedpopulation.slice(0, (((newperc*Pop).ceil.toInt)))  )
    val broadcastVar = BroadcastWrapper(  sc,  sortedpopulation.take(perc)  )
    println("Initial  brd pop length ",(broadcastVar.value).length)
    var topcrow:Array[Array[Double]]=Array.ofDim[Double](2,dim)
    topcrow(0)=sortedpopulation(0).y.clone()
    topcrow(1)=sortedpopulation(1).y.clone()
    val top2crow=BroadcastWrapper(sc,topcrow)

    val mii=BroadcastWrapper(sc,mi)
    
    var para:RDD[Crow1]=sc.parallelize(population, numSlices).persist()
    
    var fl=BroadcastWrapper(sc, population(0).flight(max_it, t))
    var cflag=false
    var delta1 :Double=0
    var delta2 :Double=0
    var l1 :Double=0
    var l2 :Double=0
    val micount=3
    var mc=0
    while(t<max_it){
      
      l1=sortedpopulation(0).fbest
      
      if(cflag==true){
        val rem=Pop-sortedpopulation.length
        val remx=List.fill(rem)(population(0).prc(sortedpopulation(0).y, sortedpopulation(scala.util.Random.nextInt(sortedpopulation.length)/*sortedpopulation.length*/).y))
        val remf=remx.map(com.dust.SCSA.testfunction.matchTest(_, casef))
        val rempop=(remx,List.range(0,rem)).zipped.map((x_p,i)=>new com.dust.SCSA.Crow1(x_p, Pop, dim, min, max,remf(i)))
        para=sc.parallelize(sortedpopulation++rempop, numSlices).persist()
        cflag=false
      }
         
    //stores the flight length  
    fl.update(population(0).flight(max_it, t))
    println("flight length ",fl.value)
    
    
    val RDD=para.mapPartitionsWithIndex{
       (idx,iterator)=>
         //variable iniatlizations 
         
         
         
         //println("Partition ",idx)
         var crows=iterator.toArray
         var bestsol=broadcastVar.value
         crows=crows ++bestsol 
         /*for(i<-0 until bestsol.length){
           crows(i)=bestsol(i)
         }*/
         var Fl=fl.value
      var node_t=0
      val node_mi=mii.value
      var top2=top2crow.value
      var topchk=crows.sortWith(_.fbest<_.fbest)//.take(2)
      var i=0   
      val partitionsize=crows.length
      while(node_t<node_mi){
        
      
         //Main loop for traversing through population
    while (i<partitionsize){
    
      
      // for finding the "out" for crow(i) with respect to all other crows   
      var i_kDiff=population(i).sumf(population(i).f,population.map(_.fbest).toArray ,0)
      var out=List.range(0,crows.length).map((j)=>crows(i).neighbor(crows(i).x, crows(j).y, crows(i).f, crows(j).fbest, i_kDiff ) )
     // var out=List.range(0,crows.length).map((j)=>crows(i).Neighbor(crows(i).x, crows(j).y ) )
      //the Mu =mean of "out"
      var mu:Double=mean(out)

      //creating Neigh and Non-Neigh
      var neigh=new Array[Int](crows.length)
      var N:Int=0
      var non_Neigh=new Array[Int](crows.length)
      var Non_N:Int=0
      
      for (z<-0 until crows.length){
       
          if(out(z)<mu){
            neigh(N)=z
            N+=1
          }else if (out(z)>=mu){
            non_Neigh(Non_N)=z
            Non_N+=1
        }
      }
      //slicing the length of the arrays
      neigh=neigh.slice(0, N)
      non_Neigh=non_Neigh.slice(0, Non_N)
      //Now we have list of Neigh and Non_Neigh contain9ing the indexes of the crows 

      var local:Int =0
      var global:Int=0
      
      try{
         //selecting random local crow 
      var randomindex:Int=scala.util.Random.nextInt(neigh.length)
      local =neigh(randomindex)  
      }
      catch{
        case e: java.lang.IllegalArgumentException => {
          println("Error in local")
          println("Value of out0 =",out(0)," out1 =",out(1)," and mu=",mu)
          local=scala.util.Random.nextInt(crows.length)
        }
        case _: java.lang.ArrayIndexOutOfBoundsException =>{
          println("Error in local")
          println("Value of out0 =",out(0)," out1 =",out(1)," and mu=",mu)
          local=scala.util.Random.nextInt(crows.length)
        }
      }
      
      try{
         //selecting global best crow from Non-Neigh
      non_Neigh=non_Neigh.sortWith(crows(_).fbest<crows(_).fbest).clone()
      
      global=non_Neigh(0)
      }
      catch{
        case e: java.lang.IllegalArgumentException => {
          println("Error in global")
          println("Value of out0 =",out(0)," out1 =",out(1)," and mu=",mu)
          var low=scala.util.Random.nextInt(crows.length)
          var high=scala.util.Random.nextInt(crows.length)
          if (low==high){
            high+=2
          }
          if(low>high){
            low=low+high
            high=low-high
            low=low-high
          }
          global=(low + scala.util.Random.nextInt()*(high - low))
        }
        
        case _: java.lang.ArrayIndexOutOfBoundsException=>{
          println("Error in global")
          println("Value of out0 =",out(0)," out1 =",out(1)," and mu=",mu)
          var low=scala.util.Random.nextInt(crows.length)
          var high=scala.util.Random.nextInt(crows.length)
          println("before set \nLow :",low," High :",high)
          if (low==high){
            high+=2
          }
          if(low>high){
            low=low+high
            high=low-high
            low=low-high
          }
          println("after set \nLow :",low," High :",high)
          global=(low + scala.util.Random.nextDouble()*(high - low)).toInt
          println("Random for global: ",global,"\n Crows length :",crows.length," for partition : ",idx)
        }
      }
      
      
 
      
	    //Deciding Strategy
	    if (crows(local).fbest<crows(global).fbest){
	      //call NLS  
	      crows(i).x=crows(i).NLS(crows(i).x ,crows(local).y,Fl)
	    }
	    else{
	      //call NGS
	      crows(i).x=crows(i).NGS(crows(i).x,crows(global).y,Fl)
	    }
      
      //to check and correct the crow
      crows(i).x=crows(i).CC(crows(i).x)
      
      //calling fitness function
      crows(i).f=com.dust.SCSA.testfunction.matchTest( crows(i).x,casef)
      
      /*
      //old strategy... but you can uncomment it and comment the PRC to check the difference in result
      //var best2=top2crow.value
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
          crows(i).x=crows(i).WAS(crows(i).x,crows(x_r).x,top2(0),ffl).toArray
        }
        val resprc=com.dust.SCSA.testfunction.matchTest( crows(i).x,casef)
          crows(i).f=resprc

      }
      */
    
     
      // Calling PRC
      //creating probability factor similar to the one in simulated anealing
      val probability =math.exp((-(math.abs(crows(i).fbest-crows(i).f)))/Fl)
      //condition for PRC
      if((probability>scala.util.Random.nextDouble()) && crows(i).f>crows(i).fbest){
        //get top crows
         //top2=top2crow.value
          // generating random crow in between the two crows
          val randcrow=crows(i).prc(top2(0),top2(1))
          val resprc=com.dust.SCSA.testfunction.matchTest( randcrow,casef)
          crows(i).x=randcrow.clone()
          crows(i).f=resprc
        
        
      }
   
      
      //Definition 1 updating memory
      if (crows(i).f<crows(i).fbest){
        crows(i).y=crows(i).x.clone()
        crows(i).fbest=crows(i).f
      }

      i+=1
      
    }
    i=0
    node_t+=1
    topchk=crows.sortWith(_.fbest<_.fbest).clone()
    val tmpres=com.dust.SCSA.testfunction.matchTest( top2(0),casef)
    if(topchk(0).fbest<tmpres){
      top2(0)=topchk(0).y
        top2(1)=topchk(1).y
    }
        
        
      }
    node_t=0
   
    
    
    //val select=((brdperc.value*crows.length).ceil.toInt)
    
   /* //replace the population at random
    for(i<-0 until selection){
      crows(0 + scala.util.Random.nextInt()*(selection - 0))=sortedpopulation(i)
    }*/
         
         
    //var selected=crows.sortWith(_.fbest<_.fbest).slice(0, select)
    var selected=/*crows.sortWith(_.fbest<_.fbest)*/topchk.take((((10.toDouble*(crows.length).toDouble/(100).toDouble))).toInt) //.take((crows.length/100)*20)
    
    //var selected=crows.slice(0, select)
         //println("select=",select)
         //broadcastVar.update(selected.clone())
         //brdtest.update(selected)
        // println("done")
         selected.toIterator
     }.persist()
    
     sortedpopulation=RDD.collect()
     //RDD.unpersist()
     sortedpopulation=sortedpopulation.sortWith(_.fbest<_.fbest).clone()
     
     
     l2=sortedpopulation(0).fbest
     var delta1=1+(l2-l1).abs
     if(t==mi){
       delta2=delta1-1
     }
     if(delta1==delta2){
       mc+=1
     }else{
       mc=0
     }
     if (mc==micount){
         cflag=true
         println("\n\npop change\n\n")
         mc=0
       }
     delta2=delta1
     
     println("population size before slice ",sortedpopulation.length)
     //sortedpopulation=sortedpopulation.slice(0, (sortedpopulation.length/(numSlices))).clone
     val sortedpopulation1=sortedpopulation.take((perc)+1).clone()
     
     println("size after slice ",sortedpopulation.length)
    broadcastVar.update(sortedpopulation1) 
    topcrow(0)=sortedpopulation(0).y.clone()
    topcrow(1)=sortedpopulation(1).y.clone()
    top2crow.update(topcrow)
     
    
      convergence(con_t)=sortedpopulation(0).fbest
      converge_address=sortedpopulation(0).y.clone()
      
      println("fbest at t=",t+mi,"is val=",convergence(con_t))
      con_t+=1
     
     

     //t=t+1
     t+=mi
    }
 
    
   
    //Printing the overall best values and the convergence list and the crow dimension values !
    println("\n\nThe Convergence ",convergence.toList)

    println("The best value out of convergence is ",convergence(con_t-1))
    //println("The best value out of convergence is ",convergence.toList.reduce((x,y)=> x min y))

    println("The best value out of convergence is ",converge_address.toList)
    //println("The of the overall best crow is :",converge_address(convergence.indexOf(convergence.toList.reduce((x,y)=> x min y))).toList,"\tGlobal best",convergence.toList.reduce((x,y)=> x min y))
    
    
  }
  
    
    
    

}  





