

package com.dust.SCSA

class Crow1(var crow:Array[Double],var pop: Int, var Dim:Int, var min:Double, var max:Double ,var fitnesses :Double)extends Serializable {
  
  //Array for crow position x at 'i'th iteration
  var x:Array[Double]=crow.clone()
  
  //Array for memory of the hiding space and the position of best fitness value
  var y:Array[Double]=x.clone()
  
  //storing the fitness value of current iteration
  var f: Double =fitnesses
  
  //storing the overall best fitness value of the crow 
  var fbest: Double =fitnesses  
  

 

  
  
  //Update memory function.... i am not using this right now...
  // Definition 1  
  def updatemem(x_i:Array[Double],m_i:Array[Double],f:Double,fbest:Double):Array[Double]={
    if(f<fbest){
      return x_i
    }
    else{
      return m_i
    }
  }
  
  
  //I was using this neighbour function in the previous functions 
  //Definition 2 generating neighborhood
  def neighbor(xi:Array[Double],x_mj:Array[Double],F:Double,Fbestj:Double,sumF:Double):Double={
    val alpha :Double=0.02
        val w=(alpha+(F-Fbestj))/sumF
        return ed(xi,x_mj)*w
  }
  
  //But now i am using this as the new neighbor function.. in this version i have removed the "w" factor 
    //Definition 2 generating neighborhood
  def Neighbor(xi:Array[Double],x_mj:Array[Double]):Double={
        return ed(xi,x_mj)
  }

    //Euclidean distance
  def ed(x:Array[Double],x_m:Array[Double]):Double={
    val diff=(x,x_m).zipped.map((a,b)=>math.pow((a-b), 2))
    return math.sqrt(diff.sum)
  }
 
  //Definition 2 for (fi - fbestk)
  def sumf(Fi:Double,Fbest:Array[Double],k:Int):Double={
    if (k<pop){
      return (Fi-Fbest(k))+sumf(Fi,Fbest,k+1)
    }
    return 0
  }
  
   //Definition 2 for (fi - fbestk)
  def sumf(Fi:Double,Fbest:Array[Double],k:Int,len:Int):Double={
    if (k<len){
      return (Fi-Fbest(k))+sumf(Fi,Fbest,k+1,len)
    }
    return 0
  }
  

  
  //NLS
  // applies NLS to ith crow by sending it towards the random neighborhood crow
  def NLS(x_i:Array[Double],m_local:Array[Double],fl:Double) :Array[Double]={
    var nls=(x_i, m_local).zipped.map((xi,mc)=> xi+fl*scala.util.Random.nextDouble()*(mc-xi)) 
    return nls
  }
  
  
  
  //The NGS algorithm is used to send the ith crow towards the global best crow
  def NGS(x_i:Array[Double],mem_global: Array[Double],fl:Double):Array[Double]={
    //select random dimensions
    var randim =scala.util.Random.nextInt(Dim) // for selecting random dims in total
    var ngs= x_i.clone()
    for(i<-0 until randim){
        var rand=scala.util.Random.nextInt(Dim)
        ngs(rand)=fl*scala.util.Random.nextDouble()*(mem_global(rand)-x_i(rand)) // for geting random indexs of dim
    }
    //var ngs=(x_i, mem_global).zipped.map((xi,mc)=> fl*scala.util.Random.nextDouble()*(mc-xi))//experiment
    return ngs
  }
  

  
  
 
  //This function makes the crow wander around his hiding spot
  // remember x_i is the mem: hiding place of the crow in the search space
  def WAS(x_i:Array[Double],x_r:Array[Double],mem_global:Array[Double],fl:Double):Array[Double]={
    var x_i_arr= x_i.clone()
    var randim =scala.util.Random.nextInt(Dim) // for selecting random dims in total
    //this is for going through random dimensions
    for(i<-0 until randim){    
      var rand=scala.util.Random.nextInt(Dim)
          x_i_arr(rand)=mem_global(rand)+fl*scala.util.Random.nextDouble()*(x_r(rand)-x_i_arr(rand)) // for geting random indexs of dim        
    }
    return CC(x_i_arr.toArray)
  }
  
  

  //check the x:Array and corrects it
  def CC(x:Array[Double]):Array[Double]={
    return x.map(a=>if(a<min || a>max){(min + scala.util.Random.nextDouble()*(max - min))} else{a})
  }
 
  
  //determines the flight lenght
  def flight(max_it:Int,t:Int):Double={
    return ( 2.02 -t *((1.08)/ max_it))
  }
  
  //To get random position value for crow version1
  def PRC():Array[Double]={
    Array.fill(Dim)((min + scala.util.Random.nextDouble()*(max - min))).toArray
  }
  
   //To get random position value for crow version1.1
  def prc(max:Double,min:Double):Array[Double]={
    Array.fill(Dim)((min + scala.util.Random.nextDouble()*(max - min))).toArray
  }
  
   //To get random position value for crow version1.2
  def prc(crow1:Array[Double],crow2:Array[Double]):Array[Double]={
    (crow1,crow2).zipped.map((c1,c2)=>if(c1<c2)(c1 + scala.util.Random.nextDouble()*(c2 - c1)) else(c2 + scala.util.Random.nextDouble()*(c1 - c2)))
  }

  
}
  
  
