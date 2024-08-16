package baka

import cec2013.Benchmark.{F1, F11, F2, F5, F9}

object testfunction {

  val f1 = F1()
  val f9 = F2()
  val f11 = F11()

  // for selecting cases for optimization functions
  def matchTest(x: Array[Double], c: String): Double = {


    //tf.test_func(x, f, dimension,population_size,func_num);
    var res: Double = 0.0

    c match {

      case "1" => {
        res = baka.oFunctions.ShiftedElliptic(x)
        res = f1(x.toVector)
      }
      case "2" => res = baka.oFunctions.ShiftedRastrigin(x)
      case "3" => res = baka.oFunctions.ShiftedAckleyFunction(x)
      case "4" => res = baka.oFunctions.Ackley(x)
      case "5" => res = baka.oFunctions.ShiftedRotatedEllipticFunction(x)
      case "6" => res = baka.oFunctions.ShiftedRotatedRastriginFunction(x)
      case "7" => res = baka.oFunctions.ShiftedRotatedAckleyFunction(x)
      case "8" => res = baka.oFunctions.ShiftedRotatedSchwefelFunction(x)
      case "9" => res = f9(x.toVector) //baka.oFunctions.NonSeparableShiftedRotatedEllipticFunction(x)
      case "10" => res = baka.oFunctions.NonSeparableShiftedRotatedRastriginFunction(x)
      case "11" => res = f11(x.toVector) //baka.oFunctions.NonSeparableShiftedRotatedAckleyFunction(x)
      case "12" => res = baka.oFunctions.NonSeparableShiftedSchwefelFunction(x)
      case "13" => res = baka.oFunctions.ShiftedRosenbrockFunction(x)
      case "14" => res = baka.oFunctions.ConformingOverlappingShiftedSchwefelFunction(x)
      case "15" => res = baka.oFunctions.ConflictingOverlappingShiftedSchwefelFunction(x)
      case "16" => res = baka.oFunctions.ShiftedSchwefelFunction(x)
      //      case "1" => res = baka.oFunctionsMillion.ShiftedElliptic(x)
      //      case "2" => res = baka.oFunctions.ShiftedRastrigin(x)
      //      case "3" => res = baka.oFunctions.ShiftedAckleyFunction(x)
      //      case "4" => res = baka.oFunctions.Ackley(x)
      //      case "5" => res = baka.oFunctions.ShiftedRotatedEllipticFunction(x)
      //      case "6" => res = baka.oFunctions.ShiftedRotatedRastriginFunction(x)
      //      case "7" => res = baka.oFunctions.ShiftedRotatedAckleyFunction(x)
      //      case "8" => res = baka.oFunctions.ShiftedRotatedSchwefelFunction(x)
      //      case "9" => res = baka.oFunctions.NonSeparableShiftedRotatedEllipticFunction(x)
      //      case "10" => res = baka.oFunctions.NonSeparableShiftedRotatedRastriginFunction(x)
      //      case "11" => res = baka.oFunctions.NonSeparableShiftedRotatedAckleyFunction(x)
      //      case "12" => res = baka.oFunctions.NonSeparableShiftedSchwefelFunction(x)
      //      case "13" => res = baka.oFunctions.ShiftedRosenbrockFunction(x)
      //      case "14" => res = baka.oFunctions.ConformingOverlappingShiftedSchwefelFunction(x)
      //      case "15" => res = baka.oFunctions.ConflictingOverlappingShiftedSchwefelFunction(x)
      //      case "16" => res = baka.oFunctions.ShiftedSchwefelFunction(x)
      /*case "1" => res=baka.oFunctionsMillion.ShiftedElliptic(x)
      case "2" => res=baka.oFunctionsMillion.ShiftedRastrigin(x)
      case "3" => res=baka.oFunctionsMillion.ShiftedAckleyFunction(x)
      case "4" => res=baka.oFunctionsMillion.Ackley(x)
      case "5" => res=baka.oFunctionsMillion.ShiftedRotatedEllipticFunction(x)
      case "6" => res=baka.oFunctionsMillion.ShiftedRotatedRastriginFunction(x)
      case "7" => res=baka.oFunctionsMillion.ShiftedRotatedAckleyFunction(x)
      case "8" => res=baka.oFunctionsMillion.ShiftedRotatedSchwefelFunction(x)
      case "9" => res=baka.oFunctionsMillion.NonSeparableShiftedRotatedEllipticFunction(x)
      case "10" => res=baka.oFunctionsMillion.NonSeparableShiftedRotatedRastriginFunction(x)
      case "11" => res=baka.oFunctionsMillion.NonSeparableShiftedRotatedAckleyFunction(x)
      case "12" => res=baka.oFunctionsMillion.NonSeparableShiftedSchwefelFunction(x)
      case "13" => res=baka.oFunctionsMillion.ShiftedRosenbrockFunction(x)
      case "14" => res=baka.oFunctionsMillion.ConformingOverlappingShiftedSchwefelFunction(x)
      case "15" => res=baka.oFunctionsMillion.ConflictingOverlappingShiftedSchwefelFunction(x)
      case "16" => res=baka.oFunctionsMillion.ShiftedSchwefelFunction(x) */
      case _ => res
    }
    //    math.abs(res)
    res
  }
}