import scala.util._
import scala.io._
import scala.collection.mutable.ListMap

object ProblemSolver {
	import Definitions._
	import Parser._
	import Polinomial._
	import Rational._
	import Factor._
	
	var circle = null : (String, Point)
	var arb = Nil : List[String]
	
	var vars = Nil : List[String]
	var semiarb = new ListMap[String, (Pol, Pol)] // map list of a point A and the fraction value of a__ 
	var determinist = new ListMap[String, ((Pol, Pol),(Pol, Pol))] // map list of a point A and the fraction value of (a,a__) 
	
	var problem = null : Problem
	
	def generateEquations(prob : Problem) : Unit = {
	  problem = prob
	   Main.append("======= Generating equations for each point ... ")
	  var c = prob.hypothesis.arbitrary.arbitrary_circle
	  circle = (c.circle_name.name,c.center)
	  
	  determinist.update(c.center.afix,((newpol("0"),newpol("1")),(newpol("0"),newpol("1"))))
	  vars = vars ::: List(c.center.afix, c.center.conjugate)
	  
	  var larb = prob.hypothesis.arbitrary.arbitrary_point_list
	  for(p <- larb)
	    arb = arb ::: List(p.p.afix)
	    
	  Main.append("\n========= List of arbitrary points:\n")
	  for(p <- arb)
	    print(p.toUpperCase + " ")
	  Main.append("")
	  
	  Main.append("========= List of semi-arbitrary points:\n")
	    
	  var lsemiarb = prob.hypothesis.semiarbitrary.semiarbitrary_point_list
	  for(q <- lsemiarb){
	    var poli = q.eq_double.equa(q.p)
	    Main.append(q.p + " is defined by equation: " + poli + " = 0")
	    poli = verifyAndSimplifyPointEquation(q.p, poli) // ===============================
   	    var solution = firstDegreeSystem(q.p.conjugate, poli)
	    Main.append("After substitutions and simplifications:\n " + q.p.conjugate + " = " + solution._1 + "  / " + solution._2 + "\n")

	    vars = vars ::: List(q.p.conjugate)

	    semiarb.update(q.p.afix, solution)	    
	  }
	  
	  Main.append("========= List of deterministic points:")
	  var ldet = prob.hypothesis.deterministic.list
	  for(q <- ldet){
		  q match{
		    case DeterministicPointSingle(p, eq_single) => {
		      var solAfix = eq_single.value
		      Main.append("\n" + p + " is a deterministic point defined by equations (After substitutions and simplifications):\n ")
		      
		      solAfix = formulaToReducedForm(solAfix._1, solAfix._2)      	      

              var solConjugate = conjugateFormula(solAfix._1, solAfix._2)
			    if(solAfix._2 == newpol("1")){
			    	Main.append(" " + p.afix + "  =  " + solAfix._1 + " \n")
			    	Main.append(" " + p.conjugate + "  =  " + solConjugate._1 + "  / " + solConjugate._2 + " \n")			    	
			    }
			    else{
			    	Main.append(" " + p.afix + "  =  " + solAfix._1 + "  / " + solAfix._2 + " \n")	
			    	Main.append(" " + p.conjugate + "  =  " + solConjugate._1 + "  / " + solConjugate._2 + " \n")
			    }
		      
		      	vars = vars ::: List(p.afix, p.conjugate)

			    determinist.update(p.afix,(solAfix,solConjugate))		      
		    }
		    case DeterministicPointDouble(p, eq1, eq2) => {
			    var poli1 = eq1.equa(p)
			    Main.append("\n" + p + " is defined by equation: " + poli1 + " = 0")
			    poli1 = verifyAndSimplifyPointEquation(p, poli1) // ===============================
			    var poli2 = eq2.equa(p)
			    Main.append("         and by equation: " + poli2 + " = 0")
			    
			    poli2 = verifyAndSimplifyPointEquation(p, poli2) // ===============================


			    var solAfix = secondDegreeSystem(p.afix,p.conjugate, poli1, poli2)
				var solConjugate = conjugateFormula(solAfix._1, solAfix._2)
			    if(solAfix._2 == newpol("1")){
			    	Main.append("After substitutions and simplifications:\n " + p.afix + "  =  " + solAfix._1)
			    	Main.append(" " + p.conjugate + "  =  " + solConjugate._1 + "  / " + solConjugate._2 + " \n")			    	
			    }
			    else{
			    	Main.append("After substitutions and simplifications:\n " + p.afix + "  =  " + solAfix._1 + "  / " + solAfix._2 )	
			    	Main.append(" " + p.conjugate + "  =  " + solConjugate._1 + "  / " + solConjugate._2 + " \n")
			    }
		      	vars = vars ::: List(p.afix, p.conjugate)
			    
			    determinist.update(p.afix,(solAfix,solConjugate))
		    }
		  }
	  }
	  
	}
	
	def conjugateFormula(pu : Pol, pd : Pol) : (Pol, Pol) = {
	  var p1 = pu.conjugate
	  var p2 = pd.conjugate
	  formulaToReducedForm(p1,p2)
	}


	//substitutes all possible values of the afixes and their conjugates in the given formula 
	//returns a simolified fraction
	def formulaToReducedForm(p11 : Pol, p22 : Pol) : (Pol, Pol) = {
	  var p1 = p11
	  var p2 = p22
	  var aux1 = null : (Pol,Pol)
	  var aux2 = null : (Pol,Pol)

	  for(v <- vars){
		if(isConjugate(v) && semiarb.contains(v.substring(0,v.length - 2))){
			var e = semiarb.get(v.substring(0,v.length - 2)).get
			aux1 = p1.subst(v, e._1, e._2)
			aux2 = p2.subst(v, e._1, e._2)
			p1 = aux1._1 * aux2._2
			p2 = aux1._2 * aux2._1
		}
        if(determinist.contains(v)){
			var e = determinist.get(v).get
		    aux1 = p1.subst(v, e._1._1, e._1._2)
		    aux2 = p2.subst(v, e._1._1, e._1._2)
		    
			p1 = aux1._1 * aux2._2
			p2 = aux1._2 * aux2._1		    
		}		
		if(isConjugate(v) && determinist.contains(v.substring(0,v.length - 2))){
			var e = determinist.get(v.substring(0,v.length - 2)).get
		    aux1 = p1.subst(v, e._2._1, e._2._2)
		    aux2 = p2.subst(v, e._2._1, e._2._2)
			p1 = aux1._1 * aux2._2
			p2 = aux1._2 * aux2._1		    
		}
		aux1 = simplifyFraction(p1,p2)
		p1 = aux1._1
		p2 = aux1._2
	  }		  
	  simplifyFraction(p1,p2)	  
	}
	

	//takes a formula of form p * P1 + p__ * P2 + P3 and reduces P1, P2 and P3 such that no conjugate appears 
	// for every semiarbitrary or deterministic points 
	def verifyAndSimplifyPointEquation(p : Point, polii : Pol) : Pol ={
	  var poli = polii
	  
	    for(v <- vars){
	      if(p.afix.compareTo(v) != 0 && p.conjugate.compareTo(v) != 0){
		      var is = false
		      for( e <- arb)
		        if(e.compareTo(v) == 0 || (e + "__").compareTo(v) == 0)
		          is = true
		      if(semiarb.contains(v)){
		        is = true
		      }
		      if(isConjugate(v) && semiarb.contains(v.substring(0,v.length - 2))){
		        is = true
		        var e = semiarb.get(v.substring(0,v.length - 2)).get
		        poli = simplifyPair(poli.subst(v, e._1, e._2))._1
		      }
		      
		      if(determinist.contains(v)){
		        is = true
		        var e = determinist.get(v).get
		        poli = simplifyPair(poli.subst(v, e._1._1, e._1._2))._1
		      }
		      if(isConjugate(v) && determinist.contains(v.substring(0,v.length - 2))){
		        is = true
		        var e = determinist.get(v.substring(0,v.length - 2)).get
		        poli = simplifyPair(poli.subst(v, e._2._1, e._2._2))._1 
		      }
		      assert(is,"Point " + v.toUpperCase + " was not defined in equation above")
	      }
	    }	  	  
	  return poli
	}
	
	
	def solveProblem : Unit = {
	  assert(problem != null, "Points equations were not generated")
	  var desired = problem.conclusion.eq_concl.equa
	  assert(desired != null, "[STRANGE ERROR] null from an equation")
	  Main.append("Conclusion is equivalent to equation: \n" + desired + " = 0\n")
	  Main.append("Attempt to prove it by substitutions and simplifications ....\n ")
	  desired = verifyAndSimplifyPointEquation(new Point("X55"), desired)
	  if(desired == newpol("0"))
   		Main.append("  !!!!!!!!!!!!!!!!!!!!!\n  !!!!!! SUCCESS !!!!!!\n  !!!!!!!!!!!!!!!!!!!!!\n\n===== Problem is correct. Proof ends here. QED ====\n")
   	  else
   	    Main.append("  FAILED\n\n===== Problem is incorrect. Sorry! ====\n")
	  
	}
}
