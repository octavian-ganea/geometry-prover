import scala.util._
import scala.io._
import java.util.StringTokenizer


object Parser{
  import Definitions._

  var code = "" //source code - will be modified during parsing
    
  def parseFile(filename : String) : Problem = {
	code = toTokens(codeFromFile(filename))
	parseProblem()
  }
  
  def codeFromFile(filename : String) : List[String] = 
    scala.io.Source.fromFile(filename, "utf-8").getLines.toList
  
  def toTokens(list : List[String]) : String ={
    var s = ""
    for(e1 <- list)
      s = s + e1.toString + " "
    val st = new StringTokenizer(s, " \t")
    var res = ""
    while(st.hasMoreTokens)
      res = res + st.nextToken + " "
    res
  }


  def jumpOver(str: String): Unit = {
    if (code(0) == ' ')
      code = code.substring(1)
    assert(code.indexOf(str) == 0, "[ERROR] code: " + code + " does not contain string : " + str)
    code = code.substring(code.indexOf(str) + str.length)
    if (code(0) == ' ')
      code = code.substring(1)
  }
   
  //problem = description + hypotehsis + conclusion
  def parseProblem() : Problem = {
	new Problem(parseDescription, parseHypothesis, parseConclusion)
  }

  def parseDescription() : Description = {
    var i = code.indexOf("Hypothesis: ")
    assert(i >=0 , "[ERROR] Hypothesis does not exists in the following code : \n" + code)
    val s = code.substring(0,i)
    code = code.substring(i)
	new Description(s)
  }
  
 
  //hypotehsis = "Hypothesis:" + arbitrary + semiarbitrary + deterministic + (constraint)?
  def parseHypothesis() : Hypothesis = {
	jumpOver("Hypothesis:")
	new Hypothesis(parseArbitrary, parseSemiarbitrary, parseDeterministic , parseConstraint)
  }
  
  //arbitrary = arbitrary_circle + (arbitrary_point)*
  def parseArbitrary() : Arbitrary = {
    var arbc = parseArbitraryCircle
    assert(arbc != null, "[ERROR] Arbitrary circle missing from the following code : \n" + code)

    var l = Nil : List[ArbitraryPoint]
    var x = parseArbitraryPoint
    while(x != null){
      l = l ::: List(x)
      x = parseArbitraryPoint
    }
    new Arbitrary(arbc, l)
  }  
  
  //arbitrary_circle = "Let an arbitrary circle " + circle_name + "(" + point + ", radix)"
  def parseArbitraryCircle() : ArbitraryCircle = {
    assert( 0 == code.indexOf("Let an arbitrary circle "), "[ERROR] Arbitrary circle missing from the following code : \n" + code)
    jumpOver("Let an arbitrary circle ")
    
    var circle_name = parseCircleName
    
    assert( 0 == code.indexOf("("), "[ERROR] Arbitrary circle defined incorrectly in the following code : \n" + code)
    jumpOver("(")

    var center = parsePoint
    jumpOver(",")
    
    var i = code.indexOf("radix")
    assert(i == 0, "[ERROR] Arbitrary circle defined incorrectly in the following code : \n" + code)
	jumpOver("radix")
	jumpOver(")")
	
    new ArbitraryCircle(circle_name, center)
  }  
  
  //arbitrary_point  = "Let an arbitrary point " + point 
  def parseArbitraryPoint() : ArbitraryPoint = {
    if( 0 != code.indexOf("Let an arbitrary point "))
      return null
    jumpOver("Let an arbitrary point ")
    new ArbitraryPoint(parsePoint)
  }  
  
  //semiarbitrary = (semiarbitrary_point)* + (euler_circle)?
  def parseSemiarbitrary() : Semiarbitrary = {
    var l = Nil : List[SemiarbitraryPoint]
    var x = parseSemiarbitraryPoint
    while(x != null){
      l = l ::: List(x)
      x = parseSemiarbitraryPoint
    }
    var e = parseEulerCircle
    new Semiarbitrary(l , e)
  }  
  
  // "Let a semiarbitrary point " + point + " situated on " + equation_hyp_double
  def parseSemiarbitraryPoint() : SemiarbitraryPoint = {
    if( 0 != code.indexOf("Let an semiarbitrary point "))
      return null
    jumpOver("Let an semiarbitrary point ")
    var p = parsePoint
    jumpOver("situated on ")
    var eq = parseEquationHypDouble
    assert(eq != null, "[ERROR] equation not defined in the code : " + code)
    new SemiarbitraryPoint(p, eq)
  }  
  
  //euler_circle = "Let the Euler circle with respect to triangle " + point + point + point + "be " + circle_name + "(omega, radix_E)"
  def parseEulerCircle() : EulerCircle = {
    if(0 != code.indexOf("Let the Euler circle "))
      return null
	jumpOver("Let the Euler circle with respect to triangle ")
	var p1 = parsePoint
	var p2 = parsePoint
	var p3 = parsePoint
	jumpOver("be ")
	var c = parseCircleName
	jumpOver("(")
	jumpOver("omega") 
	jumpOver(",")
	jumpOver("radix_E")
	jumpOver(")")
	new EulerCircle(p1,p2,p3,c)
  }  

  //deterministic = (deterministic_point_single | deterministic_point_double)*
  def parseDeterministic() : Deterministic = {
	var l = Nil : List[DeterministicPoint]
    var x = parseDeterministicPointSingle
    var x2 = null : DeterministicPointDouble
    if(x == null)
      x2 = parseDeterministicPointDouble
    
    while(x != null || x2 != null){
      if(x != null)
    	  l = l ::: List(x)
      else if(x2 != null)
          l = l ::: List(x2)
      x = parseDeterministicPointSingle
      if(x == null)
    	  x2 = parseDeterministicPointDouble      
    }
	new Deterministic(l)
  }  
  
  //deterministic_point_single = "Let a 1-equation deterministic point " + point + " situated on " + equation_hyp_single
  def parseDeterministicPointSingle() : DeterministicPointSingle = {
    if(0 != code.indexOf("Let a 1-equation deterministic point "))
      return null
	jumpOver("Let a 1-equation deterministic point ")
	var p = parsePoint
	jumpOver("situated on ")
    var eq = parseEquationHypSingle
    assert(eq != null, "[ERROR] equation not defined in the code : " + code)
    new DeterministicPointSingle(p,eq)
  }  

  //deterministic_point_double = "Let a 2-equations deterministic point " + point + " situated on " + equation_hyp_double + " and " + equation_hyp_double
  def parseDeterministicPointDouble() : DeterministicPointDouble = {
    if(0 != code.indexOf("Let a 2-equations deterministic point "))
      return null
	jumpOver("Let a 2-equations deterministic point ")
	var p = parsePoint
	jumpOver("situated on ")
    var eq1 = parseEquationHypDouble
    assert(eq1 != null, "[ERROR] equation not defined in the code : " + code)
	jumpOver("and ")
    var eq2 = parseEquationHypDouble
    assert(eq2 != null, "[ERROR] equation not defined in the code : " + code)
    new DeterministicPointDouble(p,eq1, eq2)
  }  

  //conclusion = "Conclusion:" + equation_concl
  def parseConclusion() : Conclusion = {
	assert(code.indexOf("Conclusion: ") == 0, "[ERROR] Problem has no conclusion")
	jumpOver("Conclusion: ")
	var x = parseEquationConcl
	assert(x != null, "[ERROR] Problem has no valid conclusion equation")
	new Conclusion(x)
  }  
 
  /*
     constraint = " collinear " + point + point + point
			  = " concurrent " + line + line + line
			  = line + " perpendicular on " + line |
			  = line + " parallel with " + line 
   */
  def parseConstraint() : Constraint = {
	if(code.indexOf("Constraint: ") != 0)
	  return null
	jumpOver("Constraint: ")
	if(code.indexOf("collinear ") == 0){
	  jumpOver("collinear")
	  var p1 = parsePoint
	  var p2 = parsePoint
	  var p3 = parsePoint
	  return new ConstrCollinear(p1,p2,p3)
	}
	if(code.indexOf("concurrent ") == 0){
	  jumpOver("concurrent")
	  var l1 = parseLine
	  var l2 = parseLine
	  var l3 = parseLine
	  return new ConstrConcurrent(l1,l2,l3)
	  
	}
	if(code.indexOf("perpendicular on") >= 0 && code.indexOf("perpendicular on") <= 15){
	  var l1 = parseLine
	  jumpOver("perpendicular on")
	  var l2 = parseLine
	  return new ConstrPerpendicular(l1,l2)
	}
	if(code.indexOf("parallel with") >= 0 && code.indexOf("parallel with") <= 15){
	  var l1 = parseLine
	  jumpOver("parallel with")
	  var l2 = parseLine
	  return new ConstrParallel(l1,l2)
	}
	return null
  }  
  
  /*
   * equation_hyp_single = "centroid of triangle " + point + point + point |
						  = "symmetric point of " + point + " with respect to" + point
						  = "projection of " + point + " with respect to" + line
						  = "midpoint of " + point + point
   */
  def parseEquationHypSingle() : EquationHypSingle = {
	if(code.indexOf("centroid of triangle") == 0){
	  jumpOver("centroid of triangle")
	  var p1 = parsePoint
	  var p2 = parsePoint
	  var p3 = parsePoint
	  return new EqCentroid(p1,p2,p3)
	}
	if(code.indexOf("symmetric point of ") == 0){
	  jumpOver("symmetric point of")
	  var p1 = parsePoint
	  jumpOver("with respect to")
	  var p2 = parsePoint
	  return new EqSymPointPoint(p1,p2)
	}
	if(code.indexOf("projection of ") == 0){
	  jumpOver("projection of")
	  var p1 = parsePoint
	  jumpOver("on")
	  var l = parseLine
	  return new EqProjectionPointLine(p1,l)
	}	
	if(code.indexOf("midpoint of ") == 0){
	  jumpOver("midpoint of")
	  var p1 = parsePoint
	  var p2 = parsePoint
	  return new EqMidpoint(p1,p2)  
	}
	return null
  }  

  /*
   * equation_hyp_double = line |
						  = "circle " + circle_name |
						  = line + " perpendicular on " + line |
						  = line + " paralel with " + line |
						  = " perpendicular bisector of " + line |
						  = "tangent at circle " + circle_name + " in point" + point
   */
  def parseEquationHypDouble() : EquationHypDouble = {
	if(code.indexOf("tangent at circle") == 0){
	  jumpOver("tangent at circle")
	  var circ = parseCircleName
	  jumpOver("in point")
	  var p = parsePoint
	  return new EqTangent(circ, p)
	}	

	if(code.indexOf("circle") == 0){
	  jumpOver("circle")
	  var circ = parseCircleName
	  return new EqCircle(circ)
	}	
	if(code.indexOf("perpendicular bisector of") == 0){
	  jumpOver("perpendicular bisector of")
	  var l = parseLine
	  return new EqPerpBisector(l)
	}	
	if(code.indexOf("perpendicular on") >= 0 && code.indexOf("perpendicular on") <= 15){
	  var l1 = parseLine
	  jumpOver("perpendicular on")
	  var l2 = parseLine
	  return new EqPerpendicular(l1,l2)
	}
	if(code.indexOf("parallel with") >= 0 && code.indexOf("parallel with") <= 15){
	  var l1 = parseLine
	  jumpOver("parallel with")
	  var l2 = parseLine
	  return new EqParallel(l1,l2)
	}	
	
	assert(code.indexOf("line") == 0, "[ERROR] not a valid equation double in code: " + code)
	var l = parseLine
	return new EqLine(l)
  }  

  /*
   * equation_concl = constraint | 
					= " triangle is equilateral " + point + point + point
					= " concyclic points " + point + point + point + point 
   */
  def parseEquationConcl() : EquationConcl = {
	if(code.indexOf("collinear ") == 0){
	  jumpOver("collinear")
	  var p1 = parsePoint
	  var p2 = parsePoint
	  var p3 = parsePoint
	  return new ConclCollinear(p1,p2,p3)
	}
	if(code.indexOf("concurrent ") == 0){
	  jumpOver("concurrent")
	  var l1 = parseLine
	  var l2 = parseLine
	  var l3 = parseLine
	  return new ConclConcurrent(l1,l2,l3)
	  
	}
	if(code.indexOf("perpendicular on") >= 0 && code.indexOf("perpendicular on") <= 15){
	  var l1 = parseLine
	  jumpOver("perpendicular on")
	  var l2 = parseLine
	  return new ConclPerpendicular(l1,l2)
	}
	if(code.indexOf("parallel with") >= 0 && code.indexOf("parallel with") <= 15){
	  var l1 = parseLine
	  jumpOver("parallel with")
	  var l2 = parseLine
	  return new ConclParallel(l1,l2)
	}
	if(code.indexOf("triangle is equilateral ") == 0){
	  jumpOver("triangle is equilateral")
	  var p1 = parsePoint
	  var p2 = parsePoint
	  var p3 = parsePoint
	  return new ConclEquilateral(p1,p2,p3)
	}	
	if(code.indexOf("concyclic points") == 0){
	  jumpOver("concyclic points")
	  var p1 = parsePoint
	  var p2 = parsePoint
	  var p3 = parsePoint
	  var p4 = parsePoint
	  return new ConclConcyclic(p1,p2,p3, p4)
	}	
	null
  }  
  
  //point = "A" (+ number)?, "B" (+ number)?, .....  + " "
  def parsePoint() : Point = {
	assert(Character.isUpperCase(code(0)), "[ERROR] Point name is not upper case :" + code)
	var s = code.substring(0, code.indexOf(" "))
	code = code.substring(code.indexOf(" ") + 1)
	assert(s.length == 1 ||  Character.isDigit(s(1)), "[ERROR] invalid point name at: " + s)
	new Point(s)
  }  
  
  //line = "line " + point + " " + point
  def parseLine() : Line = {
	assert(code.indexOf("line") == 0, "[ERROR] line not well defined in code:" + code)
	jumpOver("line")
	var p1 = parsePoint
	var p2 = parsePoint
	new Line(p1, p2)
  }


  //circle_name = "Circ", "Inscr", "Euler"
  def parseCircleName(): CircleName = {
    assert(0 == code.indexOf("Circ") || 0 == code.indexOf("Inscr") || 0 == code.indexOf("Euler"), "[ERROR] circle name is not one of the Circ/Inscr/Euler")
    if (code.indexOf("Circ") == 0) {
      jumpOver("Circ")
      return new CircleName("Circ")
    }
    if (code.indexOf("Inscr") == 0) {
      jumpOver("Inscr")
      return new CircleName("Inscr")
    }
    if (code.indexOf("Euler") == 0) {
      jumpOver("Euler")
      return new CircleName("Euler")
    }
    null
  }  
  
}