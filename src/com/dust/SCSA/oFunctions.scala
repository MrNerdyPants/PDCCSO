package com.dust.SCSA

import java.io.InputStream

import scala.io.Source
import scala.math._

case object oFunctions {


  val AckleyBound: Array[Int] = Array(-32, 32)
  val EllipticBound: Array[Int] = Array(-100, 100)
  val RastriginBound: Array[Int] = Array(-5, 5)
  val SchwefelBound: Array[Int] = Array(-100, 100)
  val RosenBound: Array[Int] = Array(-100, 100)

  var func = ""


  //'''''''''''''''''''''''''''''''''' Read Files ''''''''''''


  def readOptVector(name: String): Array[Double] = {
    val path = "/usr/local/Kashan/files/" + name + "-xopt.txt"
    //val stream: InputStream = getClass.getResourceAsStream(path)
    //val vectorOpt: Array[Double] = Source.fromInputStream(stream).getLines.map(_.toDouble).toArray
    //stream.close()
    val vectorOpt: Array[Double] = Source.fromFile(path).getLines.map(_.toDouble).toArray
    vectorOpt
  }

  def readPermVector(name: String): Array[Int] = {
    val path = "/usr/local/Kashan/files/" + name + "-p.txt"
   // val stream: InputStream = getClass.getResourceAsStream(path)
    //val PermVec: Array[Int] = Source.fromInputStream(stream).getLines.flatMap(x => x.split(",").map(_.toInt - 1)).toArray
    //stream.close()
    val PermVec: Array[Int] = Source.fromFile(path).getLines.map(_.toInt).toArray
    PermVec
  }

  def readSVector(name: String): Array[Int] = {
    val path = "/usr/local/Kashan/files/" + name + "-s.txt"
    //val stream: InputStream = getClass.getResourceAsStream(path)
    //val vectorS: Array[Int] = Source.fromInputStream(stream).getLines.map(_.toInt).toArray
    //stream.close()
    val vectorS: Array[Int] = Source.fromFile(path).getLines.map(_.toInt).toArray
    vectorS
  }

  def readWVector(name: String): Array[Double] = {
    val path = "/usr/local/Kashan/files/" + name + "-w.txt"
    //val stream: InputStream = getClass.getResourceAsStream(path)
    //val vectorW: Array[Double] = Source.fromInputStream(stream).getLines.map(_.toDouble).toArray
   // stream.close()
    val vectorW: Array[Double] = Source.fromFile(path).getLines.map(_.toDouble).toArray
    vectorW
  }


  def readRotationalMatrix(name: String, value: Int): Array[Array[Double]] = {
    val path = "/usr/local/Kashan/files/" + name + "-R" + value + ".txt"
    //val stream: InputStream = getClass.getResourceAsStream(path)
    //val rotationalMatrix: Array[Array[Double]] = Source.fromInputStream(stream).getLines.map(_.split(",")
      //.map(_.trim.toDouble).toArray).toArray
    //stream.close()
    val rotationalmatrix: Array[Array[String]] =Source.fromFile(path).getLines.map(_.split(",")).toArray
    val rotationalMatrix= rotationalmatrix.map(_.map(_.toDouble))
    rotationalMatrix
  }

  def hat(value: Double): Double =  {
    if  (value ==0)  0.0
    else log(abs(value))
  }

  def c1(value: Double): Double = {
    if (value > 0) 10.0 else 5.5
  }

  def c2(value: Double): Double = {
    if (value > 0) 7.9 else 3.1
  }

  lazy val vectorOptF1 = readOptVector("F1")
  lazy val vectorOptF2 : Array[Double] = readOptVector("F2")
  lazy val vectorOptF3 = readOptVector("F3")
  lazy val vectorOptF4  =  readOptVector("F4")
  lazy val r25F4        = readRotationalMatrix("F4"  ,  25)
  lazy val r50F4        = readRotationalMatrix("F4"  , 50 )
  lazy val r100F4       = readRotationalMatrix("F4"  ,  100)
  lazy val r200F4       = readRotationalMatrix("F4"  ,  200)
  lazy val sF4          = readSVector("F4")
  lazy val wF4          = readWVector("F4")
  lazy val perVectorF4  = readPermVector("F4")
  lazy val vectorOptF5  =  readOptVector("F5")
  lazy val r25F5 = readRotationalMatrix("F5"  ,  25)
  lazy val r50F5 = readRotationalMatrix("F5"  ,  50)
  lazy val r100F5 = readRotationalMatrix("F5"  ,  100)
  lazy val r200F5 = readRotationalMatrix("F5"  ,  200)
  lazy val sF5 = readSVector("F5")
  lazy val wF5 = readWVector("F5")
  lazy val perVectorF5 = readPermVector("F5")
  lazy val vectorOptF6    =   readOptVector("F6")
  lazy val r25F6          =   readRotationalMatrix("F6"  , 25 )
  lazy val r50F6          =   readRotationalMatrix("F6"  , 50 )
  lazy val r100F6         =   readRotationalMatrix("F6" , 100 )
  lazy val r200F6         =   readRotationalMatrix("F6" , 200 )
  lazy val sF6            =   readSVector("F6")
  lazy val wF6            =   readWVector("F6")
  lazy val perVectorF6    =   readPermVector("F6")
  lazy val vectorOptF7        =   readOptVector("F7")
  lazy val r25F7              =   readRotationalMatrix("F7"  ,  25)
  lazy val r50F7              =   readRotationalMatrix("F7"  ,  50)
  lazy val r100F7             =   readRotationalMatrix("F7"  ,  100)
  lazy val r200F7             =   readRotationalMatrix("F7"  ,  200)
  lazy val sF7                =   readSVector("F7")
  lazy val wF7                =   readWVector("F7")
  lazy val perVectorF7        =   readPermVector("F7")
  lazy val vectorOptF8        =   readOptVector("F8")
  lazy val r25F8              =   readRotationalMatrix("F8"  ,  25)
  lazy val r50F8              =   readRotationalMatrix("F8"  ,  50)
  lazy val r100F8             =   readRotationalMatrix("F8"  ,  100)
  lazy val r200F8             =   readRotationalMatrix("F8"  ,  200)
  lazy val sF8                =   readSVector("F8")
  lazy val wF8                =   readWVector("F8")
  lazy val perVectorF8        =   readPermVector("F8")
  lazy val vectorOptF9          =   readOptVector("F9")
  lazy val r25F9                =   readRotationalMatrix("F9"   ,  25)
  lazy val r50F9                =   readRotationalMatrix("F9"  ,  50)
  lazy val r100F9               =   readRotationalMatrix("F9"  ,  100)
  lazy val r200F9               =   readRotationalMatrix("F9"  ,  200)
  lazy val sF9                  =   readSVector("F9")
  lazy val wF9                  =   readWVector("F9")
  lazy val perVectorF9          =   readPermVector("F9")
  lazy val vectorOptF10       =   readOptVector("F10")
  lazy val r25F10             =   readRotationalMatrix("F10"  ,  25)
  lazy val r50F10             =   readRotationalMatrix("F10"  ,  50)
  lazy val r100F10            =   readRotationalMatrix("F10"  ,  100)
  lazy val r200F10            =   readRotationalMatrix("F10"  ,  200)
  lazy val sF10               =   readSVector("F10")
  lazy val wF10               =   readWVector("F10")
  lazy val perVectorF10       =   readPermVector("F10")
  lazy val vectorOptF11     =   readOptVector("F11")
  lazy val r25F11           =   readRotationalMatrix("F11"  ,  25)
  lazy val r50F11           =   readRotationalMatrix("F11"  ,  50)
  lazy val r100F11          =   readRotationalMatrix("F11"  ,  100)
  lazy val r200F11          =   readRotationalMatrix("F11"  ,  200)
  lazy val sF11             =   readSVector("F11")
  lazy val wF11             =   readWVector("F11")
  lazy val perVectorF11     =   readPermVector("F11")
  lazy val vectorOptF12 =  readOptVector("F12")
  lazy val vectorOptF13 = readOptVector("F13")
  lazy val r25F13 = readRotationalMatrix("F13", 25)
  lazy val r50F13 = readRotationalMatrix("F13", 50)
  lazy val r100F13 = readRotationalMatrix("F13", 100)
  lazy val r200F13 = readRotationalMatrix("F13", 200)
  lazy val sF13 = readSVector("F13")
  lazy val wF13 = readWVector("F13")
  lazy val perVectorF13 = readPermVector("F13")
  lazy val vectorOptF14 = readOptVector("F14")
  lazy val r25F14 = readRotationalMatrix("F14", 25)
  lazy val r50F14 = readRotationalMatrix("F14", 50)
  lazy val r100F14 = readRotationalMatrix("F14", 100)
  lazy val r200F14 = readRotationalMatrix("F14", 200)
  lazy val sF14 = readSVector("F14")
  lazy val wF14 = readWVector("F14")
  lazy val perVectorF14 = readPermVector("F14")


  lazy val VectorOptF15    =    readOptVector("F15")


  def TransformOSZ(X: Array[Double], dim: Int): Array[Double] = {

    val VectorOSZ  : Array[Double] = new Array[Double](dim)
    var i = 0
    while (i < dim) {
      VectorOSZ(i) = signum(X(i)) * exp(hat(X(i)) + 0.049 * (sin(c1(X(i)) * hat(X(i))) + sin(c2(X(i)) * hat(X(i)))))
      i += 1
    }
    VectorOSZ
  }


  def TransformASY(X: Array[Double], beta: Double, dim: Int): Array[Double] = {
    val VectorASY : Array[Double] = new Array[Double](dim)

    var i = 0
    while (i < dim) {
      if (X(i) > 0)
        VectorASY(i) = pow (X(i), 1 + beta * ((i.toDouble)/ (dim - 1).toDouble) * sqrt(X(i)))
      else
        VectorASY(i) = X(i)
      i += 1
    }
    VectorASY
  }

  def Lambda(X: Array[Double], alpha: Double, dim: Int): Array[Double] = {
    val VectorLamda : Array[Double] = new Array[Double](dim)
    var i = 0
    while (i < dim) {
      VectorLamda(i) = X(i) * pow(alpha, 0.5 * ((i.toDouble )/ (dim - 1).toDouble))
      i += 1
    }
    VectorLamda
  }

  def SubtractFromOrigional(X: Array[Double], Y: Array[Double], dim: Int): Array[Double] = {

    val VectorXY :Array[Double] = new Array[Double](dim)
    var i = 0
    while (i < dim) {
      VectorXY(i) = X(i) - Y(i)
      i += 1
    }
    VectorXY
  }


 

  def ShiftedElliptic(listX: Array[Double]): Double = {


    val VectorOpt: Array[Double] = vectorOptF1

    val VectorY :Array[Double]= SubtractFromOrigional(listX, VectorOpt, listX.length)
    ShiftedElliptic12 (  VectorY , listX.length  )

  }
  def ShiftedElliptic12(VectorY: Array[Double] , dim  : Int ): Double = {

    val VectorZ :Array[Double]= TransformOSZ(VectorY, dim)
    Elliptic(VectorZ, dim)

  }
  def Elliptic(arr: Array[Double], dim: Int): Double = {
    var result : Double= 0.0
    var i = 0
    while (i < dim) {
      result += pow(1.0e6, i.toDouble / (dim - 1)) * arr(i) * arr(i)
      i += 1
    }
    result
  }



  def ShiftedRastrigin (  listX : Array[Double]  ) : Double  =  {
    val VectorOpt : Array[Double] =  vectorOptF2

    val dimn   =   listX.length
    val VectorY1 : Array[Double] =  SubtractFromOrigional(listX, VectorOpt, dimn)
    ShiftedRastrigin12 ( VectorY1 , dimn)

  }
  def ShiftedRastrigin12 (  VectorY : Array[Double] , dim : Int  ) : Double  =  {
    val power = 10
    val beta  : Double =   0.2

    val VectorOSZ   :Array[Double]        =     TransformOSZ(VectorY, dim)
    val VectorASY   : Array[Double]       =     TransformASY(  VectorOSZ   ,  beta   ,   dim )
    val VectorZ     : Array[Double]       =     Lambda   (  VectorASY , power  ,   dim )
    //println("vector osz",VectorOSZ.toList,"\nVectorASY",VectorASY.toList,"\nVectorZ",VectorZ.toList)
    Rastrigin (VectorZ , dim)
  }

  def Rastrigin ( arr : Array [Double] , dim : Int ) : Double ={
    var result : Double= 0.0
    var i = 0
    while ( i < dim ) {
      result += arr(i) * arr(i) - 10.0 * cos(2 * Pi * arr(i)) + 10.0
      i  +=  1
    }
    result
  }

  

  def ShiftedAckleyFunction   (    listX : Array[Double]   ) : Double  =  {
    val VectorOpt : Array[Double] =  vectorOptF3
    val dimm   =   listX.length
    val VectorY    : Array[Double]  =     SubtractFromOrigional(listX, VectorOpt, dimm)
    ShiftedAckleyFunction12( VectorY , dimm )
  }
  def ShiftedAckleyFunction12   (    VectorY : Array[Double] , dim : Int ) : Double  =  {
    val beta : Double =   0.2
    val power = 10
    val VectorOSZ  : Array[Double]  =     TransformOSZ(VectorY, dim)
    val VectorASY   : Array[Double] =     TransformASY(  VectorOSZ   ,  beta   ,   dim )
    val VectorZ     : Array[Double] =     Lambda  (  VectorASY , power  ,   dim )
    Ackley  (  VectorZ     )
  }


  def Ackley(arr: Array[Double]): Double = {

    val d = arr.length
    var r1 : Double= 0.0
    var r2 : Double= 0.0
    var i = 0
    while (i < d) {
      r1 += arr(i) * arr(i)
      r2 += cos(2.0 * Pi * arr(i))
      i += 1
    }
    -20.0 * exp(-0.2 * sqrt(r1 / d.toDouble)) - exp(r2 / d.toDouble) + 20.0 + E
  }



  def matrixVectorProduct (matrix: Array[Array[Double]], vector: Array[Double] , dim : Int): Array[Double] = {
    val result  :Array[Double] = new Array[Double](dim)
    var i = 0
    while (i < dim) {
      result(i) = 0.0
      var j = 0
      while (j < dim) {
        result(i) += vector(j) * matrix(i)(j)
        j += 1
      }
      i += 1
    }
    result
  }


  def ShiftedRotatedEllipticFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF4
    val dim = VectorOpt.length
    val permVector : Array[Int]  =   perVectorF4
    val r25 :  Array[Array[Double]]  = r25F4
    val r50 :  Array[Array[Double]]  = r50F4
    val r100:  Array[Array[Double]]  =r100F4
    val r200:  Array[Array[Double]]  =r200F4
    val s: Array[Int] =    sF4
    val w: Array[Double] = wF4

    val VectorY  :Array[Double] =   SubtractFromOrigional(listX, VectorOpt, dim)  //  anotherZ
    var count = 0
    var sum1 : Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated : Array[Double] =  rotateVector(   i  , count ,permVector ,VectorY , s , r25 , r50 , r100 ,r200 )
      count = count + s(i)
      sum1 += w(i) * ShiftedElliptic12 (   rotated , s(i) )
      i += 1
    }

    val z  : Array[Double] = new Array[Double](dim - count )
    var x = count

    while (x < dim){
      z(x - count ) =  VectorY (   permVector   (  x  )   )
      x += 1
    }

    sum1 +=  ShiftedElliptic12 (  z  , dim - count )
    sum1

  }

  
  def rotateVector   (  i : Int ,   c : Int  , pVector : Array[Int] , vecY: Array[Double] , ss : Array[Int] ,
                        rr25: Array[Array[Double]] ,rr50 : Array[Array[Double]] , rr100 : Array[Array[Double]],
                        rr200 : Array[Array[Double]]
                     ) : Array  [Double]  = {
    val z  : Array[Double] = new Array[Double](ss(i))
    var j = c
    while ( j < c + ss(i)){
      z (j-c) = vecY ( pVector ( j ) )
      j += 1
    }


    ss(i)   match {
      case 25 =>    matrixVectorProduct (  rr25    , z ,ss(i) )
      case 50 =>    matrixVectorProduct (  rr50    , z  ,ss(i))
      case 100 =>    matrixVectorProduct (  rr100  , z  ,ss(i) )
      case 200 =>    matrixVectorProduct (  rr200  , z  ,ss(i) )
    }
  }



 

  def ShiftedRotatedRastriginFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF5
    val dim = VectorOpt.length
    val permVector: Array[Int] = perVectorF5

    val r25 :  Array[Array[Double]]  = r25F5
    val r50 :  Array[Array[Double]]  = r50F5
    val r100:  Array[Array[Double]]  =r100F5
    val r200:  Array[Array[Double]]  =r200F5
    val s: Array[Int] = sF5
    val w: Array[Double] = wF5

    val VectorY  : Array[Double] =   SubtractFromOrigional(listX, VectorOpt, dim)  //  anotherZ

    var count = 0

    var sum1 : Double = 0.0

    var j = 0
    while (j < s.length) {
      val rotated : Array[Double] = rotateVector(   j , count ,permVector ,VectorY , s , r25 , r50 , r100 , r200 )
      count = count + s(j)
      sum1 += w(j) * ShiftedRastrigin12 (   rotated ,s(j) )
      j += 1
    }

    val z = new Array[Double](dim - count)
    var i = count
    while (i < dim) {
      z(i - count ) =  VectorY (   permVector   (  i  )   )
      i += 1
    }

    sum1 +=  ShiftedRastrigin12 (  z , dim - count)
    sum1
  }


  def ShiftedRotatedAckleyFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF6
    val dim = VectorOpt.length
    val VectorY :Array[Double] = SubtractFromOrigional(listX, VectorOpt, dim)

    val permVector: Array[Int] =perVectorF6

    val r25 :  Array[Array[Double]]  = r25F6
    val r50 :  Array[Array[Double]]  = r50F6
    val r100:  Array[Array[Double]]  = r100F6
    val r200:  Array[Array[Double]]  = r200F6
    val s: Array[Int] = sF6
    val w: Array[Double] = wF6
    var count = 0
    var sum1 :Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated  :Array[Double] = rotateVector(   i  , count ,permVector ,VectorY , s , r25 , r50 , r100 , r200  )
      count = count + s(i)
      sum1 += w(i) * ShiftedAckleyFunction12 (   rotated  , s(i) )
      i += 1
    }
    val z :Array[Double] = new Array[Double](dim - count)
    var j = count
    while (j < dim) {
      z(j - count ) =  VectorY (   permVector   (  j  )   )
      j += 1
    }
    sum1 +=  ShiftedAckleyFunction12 (  z  , dim - count )
    sum1
  }


  def Sphere (arr : Array[ Double ] , dim : Int) : Double ={
    var sum = 0.0
    var i = 0
    while (i < dim) {
      sum += pow( arr(i),2)
      i += 1
    }
    sum
  }




  def ShiftedRotatedSchwefelFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF7
    val dim = listX.length
    val VectorY :Array[Double] = SubtractFromOrigional(listX , VectorOpt , dim)

    val permVector: Array[Int] = perVectorF7

    val r25 :  Array[Array[Double]]  = r25F7
    val r50 :  Array[Array[Double]]  = r50F7
    val r100:  Array[Array[Double]]  = r100F7
    val r200:  Array[Array[Double]]  = r200F7
    val s: Array[Int] = sF7
    val w: Array[Double] = wF7

    var count = 0
    var sum1 : Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated  :Array[Double] = rotateVector(   i  , count ,permVector ,VectorY ,s ,r25 , r50 , r100 , r200  )
      count = count + s(i)
      sum1 += w(i) * ShiftedSchwefelFunction12 (   rotated , s(i) )
      i += 1
    }

    val z = new Array[Double](dim - count)
    var j = count
    while (j < s.length) {
      z( j - count ) =  VectorY (   permVector   (  j  )   )
      j += 1
    }

    sum1 +=  Sphere (  z , z.length )
    sum1
  }



  def NonSeparableShiftedRotatedEllipticFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF8
    val dim = listX.length
    val VectorY :Array[Double] = SubtractFromOrigional(listX , VectorOpt , dim)

    val permVector: Array[Int] = perVectorF8

    val r25 :  Array[Array[Double]]  = r25F8
    val r50 :  Array[Array[Double]]  = r50F8
    val r100:  Array[Array[Double]] =r100F8
    val r200:  Array[Array[Double]] =r200F8
    val s: Array[Int] = sF8
    val w: Array[Double] = wF8

    var count = 0

    var sum1 : Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated  :Array[Double] = rotateVector(   i  , count ,permVector , VectorY , s ,r25 , r50 ,r100 ,r200 )
      count = count + s(i)
      sum1 += w(i) * ShiftedElliptic12 (   rotated  , s(i))
      i += 1
    }
    sum1
  }


  def NonSeparableShiftedRotatedRastriginFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF9
    val dim = listX.length
    val VectorY :Array[Double] = SubtractFromOrigional(listX , VectorOpt , dim)

    val permVector: Array[Int] = perVectorF9

    val r25 :  Array[Array[Double]]  = r25F9
    val r50 :  Array[Array[Double]]  = r50F9
    val r100:  Array[Array[Double]] =r100F9
    val r200:  Array[Array[Double]] =r200F9
    val s: Array[Int] = sF9
    val w: Array[Double] = wF9

    var count = 0


    var sum1 : Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated :Array[Double] = rotateVector(   i  , count ,permVector , VectorY , s ,r25 ,r50,r100 ,r200 )
      count = count + s(i)
      sum1 += w(i) * ShiftedRastrigin12 (   rotated , s(i) )
      i += 1
    }
    sum1
  }



  def NonSeparableShiftedRotatedAckleyFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF10
    val dim = listX.length
    val VectorY  :Array[Double] = SubtractFromOrigional(listX , VectorOpt , dim)

    val permVector: Array[Int] = perVectorF10

    val r25 :  Array[Array[Double]]  = r25F10
    val r50 :  Array[Array[Double]]  = r50F10
    val r100:  Array[Array[Double]]  = r100F10
    val r200:  Array[Array[Double]]  = r200F10
    val s: Array[Int] = sF10
    val w: Array[Double] = wF10

    var count = 0
    def rotateVector   (  ii : Int ,   c : Int ) : Array  [Double]  = {
      val z :Array[Double] = new Array[Double](s(ii))
      var j = c
      while (j <  c + s(ii)){
        z (j-c) = VectorY ( permVector ( j ) )
        j += 1
      }


      s(ii)   match {
        case 25 =>    matrixVectorProduct (  r25    , z ,s(ii) )
        case 50 =>    matrixVectorProduct (  r50    , z  ,s(ii))
        case 100 =>    matrixVectorProduct (  r100  , z  ,s(ii) )
        case 200 =>    matrixVectorProduct (  r200  , z  ,s(ii) )
      }
    }


    var sum1 : Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated :Array[Double] = rotateVector(   i  , count   )
      count = count + s(i)
      sum1 += w(i) * ShiftedAckleyFunction12 (   rotated , s(i) )
      i += 1
    }
    sum1
  }




  def NonSeparableShiftedSchwefelFunction(listX : Array[Double]  ) : Double  = {
    val VectorOpt: Array[Double] = vectorOptF11
    val dim = listX.length
    val VectorY : Array[Double] = SubtractFromOrigional(listX , VectorOpt , dim)

    val permVector: Array[Int] = perVectorF11

    val r25 :  Array[Array[Double]]  =  r25F11
    val r50 :  Array[Array[Double]]  =  r50F11
    val r100:  Array[Array[Double]]  =  r100F11
    val r200:  Array[Array[Double]]  =  r200F11
    val s: Array[Int] = sF11
    val w: Array[Double] = wF11

    var count = 0

    var sum1 : Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated :Array[Double] = rotateVector(   i  , count ,permVector , VectorY , s ,r25 , r50, r100 ,r200 )
      count = count + s(i)
      sum1 += w(i) * ShiftedSchwefelFunction12 (   rotated ,  s(i) )
      i += 1
    }
    sum1
  }


  

  def ShiftedRosenbrockFunction(listX : Array[Double]  ) : Double  = {

    val VectorOpt: Array[Double] = vectorOptF12
    val dim = listX.length
    val VectorY: Array[Double] = SubtractFromOrigional(listX, VectorOpt, dim)
    Rosen(VectorY, dim)

  }
    def Rosen(arr: Array[Double] , dim : Int) : Double = {

      var s: Double = 0.0
      var j = 0
      while (j < dim - 1 ) {
        s += 100 * pow((pow(arr(j), 2) - arr(j + 1)), 2) + pow(arr(j) - 1, 2)
        j+= 1
      }
      s
    }


  def ConformingOverlappingShiftedSchwefelFunction(listX: Array[Double]): Double = {
    val VectorOpt: Array[Double] = vectorOptF13
    val dim = VectorOpt.length // 905
    val VectorY: Array[Double] = SubtractFromOrigional(listX, VectorOpt, dim)

    val permVector: Array[Int] = perVectorF13

    val r25: Array[Array[Double]] = r25F13
    val r50: Array[Array[Double]] = r50F13
    val r100: Array[Array[Double]] = r100F13
    val r200: Array[Array[Double]] = r200F13
    val s: Array[Int] = sF13
    val w: Array[Double] = wF13

    var count = 0
    val m = 10 // m == overlap size

    //overlapping rotate
    def rotateVector(l: Int, c: Int): Array[Double] = {

      val z: Array[Double] = new Array[Double](s(l))
      var j = c - l * m
      while (j < (c + s(l) - (l * m))  ){
        z(j - (c - l * m)) = VectorY(permVector(j))
        j += 1
      }

      s(l) match {
        case 25 => matrixVectorProduct(r25, z, s(l))
        case 50 => matrixVectorProduct(r50, z, s(l))
        case 100 => matrixVectorProduct(r100, z, s(l))
        case 200 => matrixVectorProduct(r200, z, s(l))
      }
    }

    var sum1: Double = 0.0

    var i = 0
    while (i < s.length) {
      val rotated: Array[Double] = rotateVector(i, count)
      count = count + s(i)
      sum1 += w(i) * ShiftedSchwefelFunction12(rotated, s(i))
      i += 1
    }
      sum1

  }



  def ConflictingOverlappingShiftedSchwefelFunction(listX: Array[Double]): Double = {
    val VectorOpt: Array[Double] = vectorOptF14
    val permVector: Array[Int] = perVectorF14

    val r25: Array[Array[Double]] = r25F14
    val r50: Array[Array[Double]] = r50F14
    val r100: Array[Array[Double]] = r100F14
    val r200: Array[Array[Double]] = r200F14

    val s: Array[Int] = sF14
    val w: Array[Double] = wF14

    var count : Int = 0
    val m = 10 // m == overlap size
    val newOpt : Array[Array[Double]] = Array.ofDim[Double](s.length, s.max)

    var inc = 0
    var i = 0
    while (i < s.length){
      var b = 0
      while (b < s(i)){
        newOpt(i)(b) = VectorOpt(inc + b)
        b += 1
      }
      inc += s(i)
      i += 1
    }

    def rotateVector(i: Int, c: Int): Array[Double] = {

      val z : Array[Double] = new Array[Double](s(i))
      var j = c - i * m
      while (j < (  c + s(i) - (i * m ) ) ){
        z (j-(c - i* m)) = listX(permVector(j)) - newOpt(i)(j-(c - i* m ))
        j += 1
      }


      count = count + s(i)
      s(i) match {
        case 25 => matrixVectorProduct(r25, z , s(i))
        case 50 => matrixVectorProduct(r50, z , s(i))
        case 100 => matrixVectorProduct(r100, z , s(i))
        case 200 => matrixVectorProduct(r200, z , s(i))
      }
    }

    var sum1 : Double = 0.0

    var l = 0
    while (l < s.length) {
      val rotated : Array[Double] = rotateVector(   l  , count   )
      sum1 += w(l) * ShiftedSchwefelFunction12 (   rotated , s(l) )
      l += 1
    }
    sum1
  }





  def Schwefel ( arr : Array[Double] , dim : Int) : Double ={

    var s1 : Double = 0.0
    var s2 : Double = 0.0
    var i = 0
    while (i < dim) {
      s1 += arr(i)
      s2 += s1 * s1
      i += 1
    }
    s2
  }

  def ShiftedSchwefelFunction (  listX : Array[Double]  ) : Double  =  {
    val VectorOpt : Array[Double] =  VectorOptF15
    val dimn             =     listX.length
    val beta            =     0.2
    val VectorY         =     SubtractFromOrigional(listX , VectorOpt , dimn)
    ShiftedSchwefelFunction12(VectorY , dimn)
  }

  def ShiftedSchwefelFunction12 (  VectorY : Array[Double]  , dim : Int  ) : Double  =  {
    val beta            =     0.2
    val VectorOSZ       =     TransformOSZ(VectorY , dim)
    val VectorASY       =     TransformASY  (  VectorOSZ   ,  beta   ,   dim )
    Schwefel   (   VectorASY   , dim )
  }

}



