package com.dust.SCSA




object testfunction {
    
  // for selecting cases for optimization functions 
def matchTest( x:Array[Double],c: String): Double  = 
  { 
  

//tf.test_func(x, f, dimension,population_size,func_num);
     var res:Double=0.0
  c match {
       case "1" => res=com.dust.SCSA.oFunctionsMillion.ShiftedElliptic(x)
  case "2" => res=com.dust.SCSA.oFunctions.ShiftedRastrigin(x)
  case "3" => res=com.dust.SCSA.oFunctions.ShiftedAckleyFunction(x) 
  case "4" => res=com.dust.SCSA.oFunctions.Ackley(x) 
  case "5" => res=com.dust.SCSA.oFunctions.ShiftedRotatedEllipticFunction(x) 
  case "6" => res=com.dust.SCSA.oFunctions.ShiftedRotatedRastriginFunction(x) 
  case "7" => res=com.dust.SCSA.oFunctions.ShiftedRotatedAckleyFunction(x) 
  case "8" => res=com.dust.SCSA.oFunctions.ShiftedRotatedSchwefelFunction(x) 
  case "9" => res=com.dust.SCSA.oFunctions.NonSeparableShiftedRotatedEllipticFunction(x) 
  case "10" => res=com.dust.SCSA.oFunctions.NonSeparableShiftedRotatedRastriginFunction(x) 
  case "11" => res=com.dust.SCSA.oFunctions.NonSeparableShiftedRotatedAckleyFunction(x) 
  case "12" => res=com.dust.SCSA.oFunctions.NonSeparableShiftedSchwefelFunction(x) 
  case "13" => res=com.dust.SCSA.oFunctions.ShiftedRosenbrockFunction(x) 
  case "14" => res=com.dust.SCSA.oFunctions.ConformingOverlappingShiftedSchwefelFunction(x) 
  case "15" => res=com.dust.SCSA.oFunctions.ConflictingOverlappingShiftedSchwefelFunction(x) 
  case "16" => res=com.dust.SCSA.oFunctions.ShiftedSchwefelFunction(x) 
  /*case "1" => res=com.dust.SCSA.oFunctionsMillion.ShiftedElliptic(x)
  case "2" => res=com.dust.SCSA.oFunctionsMillion.ShiftedRastrigin(x)
  case "3" => res=com.dust.SCSA.oFunctionsMillion.ShiftedAckleyFunction(x) 
  case "4" => res=com.dust.SCSA.oFunctionsMillion.Ackley(x) 
  case "5" => res=com.dust.SCSA.oFunctionsMillion.ShiftedRotatedEllipticFunction(x) 
  case "6" => res=com.dust.SCSA.oFunctionsMillion.ShiftedRotatedRastriginFunction(x) 
  case "7" => res=com.dust.SCSA.oFunctionsMillion.ShiftedRotatedAckleyFunction(x) 
  case "8" => res=com.dust.SCSA.oFunctionsMillion.ShiftedRotatedSchwefelFunction(x) 
  case "9" => res=com.dust.SCSA.oFunctionsMillion.NonSeparableShiftedRotatedEllipticFunction(x) 
  case "10" => res=com.dust.SCSA.oFunctionsMillion.NonSeparableShiftedRotatedRastriginFunction(x) 
  case "11" => res=com.dust.SCSA.oFunctionsMillion.NonSeparableShiftedRotatedAckleyFunction(x) 
  case "12" => res=com.dust.SCSA.oFunctionsMillion.NonSeparableShiftedSchwefelFunction(x) 
  case "13" => res=com.dust.SCSA.oFunctionsMillion.ShiftedRosenbrockFunction(x) 
  case "14" => res=com.dust.SCSA.oFunctionsMillion.ConformingOverlappingShiftedSchwefelFunction(x) 
  case "15" => res=com.dust.SCSA.oFunctionsMillion.ConflictingOverlappingShiftedSchwefelFunction(x) 
  case "16" => res=com.dust.SCSA.oFunctionsMillion.ShiftedSchwefelFunction(x) */
  case _ => res
}
     math.abs(res)
  }
}