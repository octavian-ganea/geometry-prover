import java.lang.StringBuffer
import java.io._
import scala.collection.mutable.ListMap
import java.util.StringTokenizer

object Polinomial{
  import Rational._
  import Factor._

  var restrictions = new ListMap[String, Int]
  
  def singlepol(s : String) : Pol = newpol("1 * " + s)
  def newpol(s : String) : Pol = {
	  var l = Nil : List[Factor]
	  var tk = new StringTokenizer(s, "+")
	  while(tk.hasMoreTokens)
	    l = l ::: List(newfactor(tk.nextToken).simplify)
	  new Pol(l)
  }
  
  //Pol
  case class Pol(val fffactors : List[Factor]) extends Formula{
	  def factors = fffactors
	  
	  override def toString : String = {
	    var me = this.simplify
	    var s = ""
	    for ( e <- me.factors )
	      if(e.c.u > 0)
	    	  s = s + " + " + e.toString 
	    for ( e <- me.factors )
	      if(e.c.u < 0)
	          s = s + e.toString
	    if(s.size >=3 && s.substring(0,3).compareTo(" + ") == 0)
	    	s = s.substring(3)
	    if(s.size == 0)
	      s = "0"
	    s
	  }

	  def conjugate : Pol = {
	    var l = Nil : List[Factor]
	    for (v <- this.factors)
	      l = l ::: List(v.conjugate)
	    new Pol(l)
	  }
	  
	  def *(f : Factor) : Pol= {
	    var l = Nil : List[Factor]
	    for(e <- factors)
	      l = l ::: List(e * f)
	    (new Pol(l)).simplify
	  }

	  //collapses all identical factors : for example : 2 * a + -1 * a ---> 1 * a
	  //reduces 0 terms
	    def simplify: Pol = {
	      var l = Nil : List[Factor]
	      for(e <- factors){
	        var fac = new Factor(new Rational(0), e.list.clone)
	        for(e2 <- factors)
	          if(new Factor(new Rational(1), fac.list) == new Factor(new Rational(1), e2.list))
	            fac = new Factor(fac.c + e2.c, fac.list)
	        if(fac.c.u != 0)
	        	l = l ::: List(fac)
	      }
   	      var finallist = Nil : List[Factor]
	      for(e1 <- l){
	        var b = true
	        for(e2 <- finallist)
	          if(e2 == e1)
	            b = false
	        if(b == true)
	          finallist = finallist ::: List(e1)
	      }
	    	 
	      new Pol(finallist)
	    }
	
	  def +(p : Pol) : Pol = {
	    return (new Pol(factors ::: p.factors)).simplify
	  }
	  
	  def unary_- : Pol = {
	    var f = Nil : List[Factor]
	    for(x <- factors)
	      f = f ::: List(new Factor(-x.c, x.list.clone))
	    new Pol(f)
	  }
	  
	  def -(p : Pol) : Pol = {
	    return (this + (-p)).simplify
	  }
	  
	  def *(p : Pol) : Pol = {
	    var res = new Pol(Nil : List[Factor])
	    for(e <- factors)
	      res = res + (p * e)
	    res.simplify
	  }
	    
	  def ^(n : Int) : Pol = {
	    if(n == 0)
	      return newpol("1")
	    var res = this
	    for( i <- 2 to n)
	      res = res * this
	    res
	  }
	  
	  def ==(p : Pol) : Boolean = {
	    if(null == p)
	      return false
	    var p1 = this.simplify
	    var p2 = p.simplify
	    if(p1.factors.size != p2.factors.size)
	      return false
	    for(e1 <- p1.factors){
	      var b = false
	      for(e2 <- p2.factors)
	        if(e1 == e2)
	          b = true
	      if(b == false)
	        return false
	    }
	    return true
	  }

	  def !=(p : Pol) : Boolean = !(this == p)
	  
	  def subst(a : String, b: String) : Pol = {
	    var l  = Nil : List[Factor]
	    for(f <- factors)
	      l = l ::: List(f.subst(a,b))
	    (new Pol(l)).simplify
	  }

	  def MaxDegreeOfVar(z: String): Int = {
	    var i = 0
	    for (f <- factors)
	      if(f.degreeOfVar(z) > i)
	        i = f.degreeOfVar(z)
	    i
	  }

	  def MinDegreeOfVar(z: String): Int = {
	    var i = Integer.MAX_VALUE
	    for (f <- factors)
	      if(f.degreeOfVar(z) < i)
	        i = f.degreeOfVar(z)
	    i
	  }

	  //divides every factor from the polinomial by z^n
	  //n can also be negative !!!!!
	  def divideByVar(z : String, n : Int) : Pol = {
	    assert(MinDegreeOfVar(z) >= n, "[ERROR] exponent from divide is too large z:" + z + " n:" + n)
	    var l = Nil :  List[Factor]
	    for(e <- factors)
	      l = l ::: List(e.divide(z,n))
	    new Pol(l)
	  }
	  
	  def listOfAllVars : ListMap[String, Int] = {
	    var l = new ListMap[String, Int]
	    for(f <- factors)
	      for(e <- f.list)
	        l.update(e._1,1)
	    l
	  }
	  
	  //substitutes in current Polinomial var a with fraction pu/pd and returns a fraction (up / down) of polinomials
	  //tries to reduce the fraction up/down with variables ^ maximum power
	  def subst(a: String, pu : Pol, pd : Pol) : (Pol, Pol) = {
	    var max = this.MaxDegreeOfVar(a)
	    var res  = new Pol(List())
	    for(f <- factors){
	    	var k = f.degreeOfVar(a)
	    	var ff = f.list.clone
	    	ff.remove(a)
	    	res = res + (new Pol(List(new Factor(f.c, ff))) * (pu ^ k) * (pd ^ (max - k)))
	    }
	    res = res.simplify
	    var down = pd ^ max

	    (res, down)
	  }

	  //decomposes pol after variable a : pol = a * p1 + p2
	  def firstDegreeDecomp(a : String) : (Pol, Pol) = {
	    assert(MaxDegreeOfVar(a) <= 1, "[SORRY] degree > 1 at point " + a + " of deg: " + MaxDegreeOfVar(a) + " at poli: " + this)
	    var l1 = Nil : List[Factor]
	    var l2 = Nil : List[Factor]
	    for(f <- factors)
	    	if(f.containsVar(a))
	    	  l1 = l1 ::: List(f.formulaWithoutOne(a))
	    	else
	    	  l2 = l2 ::: List(f)
	    (new Pol(l1), new Pol(l2))
	  }
 
	  def firstFactorWith(a: String) : Factor ={
	    for(f <- factors)
	      if(f.containsVar(a))
	        return f
	    null
	  }
	  
	  //decomposes polinomial by factor (a-b) if this is possible
	  def decomposeByFactor(a : String, b: String) : Pol = {
	    if(subst(a,b) != newpol("0"))
	      return null
	    if(this.simplify == newpol("0"))
	      return this.simplify
	    var before = this.factors
	    var after = Nil : List[Factor]
	    var aux = Nil : List[Factor]
	    var f = null : Factor
	    while(before.size > 0){
	      f = (new Pol(before)).firstFactorWith(a)
	      aux = Nil
	      for(el <- before)
	        if(el != f)
	          aux = aux ::: List(el)
	      aux = aux ::: List(f.divide(a,1) * newfactor("1 * " + b))
	      before = new Pol(aux).simplify.factors
	      after = after ::: List(f.divide(a,1))
	    }
	    new Pol(after).simplify
	  }

	  //decomposes polinomial by factor (a+b) if this is possible
	  def decomposeBySum(a : String, b: String) : Pol = {
	    if(subst(a,newpol("-1 * " + b), newpol("1"))._1 != newpol("0"))
	      return null
	    if(this.simplify == newpol("0"))
	      return this.simplify
	    var before = this.factors
	    var after = Nil : List[Factor]
	    var aux = Nil : List[Factor]
	    var f = null : Factor
	    while(before.size > 0){
	      f = (new Pol(before)).firstFactorWith(a)
	      aux = Nil
	      for(el <- before)
	        if(el != f)
	          aux = aux ::: List(el)
	      aux = aux ::: List(f.divide(a,1) * newfactor("-1 * " + b))
	      before = new Pol(aux).simplify.factors
	      after = after ::: List(f.divide(a,1))
	    }
	    new Pol(after).simplify
	  }	  
	  
  }
  
//=========================== very important functions ===========================
  	def simplifyFractionByConstants(p1 : Pol, p2 : Pol) : (Pol, Pol) = {
	    var lcm = 0
	    for(f <- p2.factors)
	      lcm = cmmdc(lcm, f.c.u)
	    for(f <- p1.factors)
	      lcm = cmmdc(lcm, f.c.u)  
	    
	    var list1 = Nil : List[Factor]
	    var list2 = Nil : List[Factor]
	    for(f <- p2.factors)
	      list2 = list2 ::: List(new Factor(f.c / lcm,f.list))
	    for(f <- p1.factors)
	      list1 = list1 ::: List(new Factor(f.c / lcm,f.list))
	    (new Pol(list1), new Pol(list2))
  	}
  	
	  //simplify fraction by a^k or (a-b)^k or (a + b)^k
	  def simplifyFraction(pu : Pol, pd : Pol) : (Pol, Pol) = {
	    var allVars = pu.listOfAllVars 
	    var p1 = pu
	    var p2 = pd
	    for (v <- allVars){
	    	var d1 = p1.MinDegreeOfVar(v._1)
	    	var d2 = p2.MinDegreeOfVar(v._1)
	    	var d = d1
	    	if(d1 > d2)
	    	  d = d2
	    	p1 = p1.divideByVar(v._1,d)
	    	p2 = p2.divideByVar(v._1,d)
	    }	    
	    var l = Nil : List[(String,String)]
	    for(e1 <- allVars)
	      for(e2 <- allVars)
	        if(e1._1.compareTo(e2._1) > 0)
	          l = l ::: List((e1._1,e2._1))
	    for((a,b) <- l)
	      while(p1.subst(a,b) == newpol("0") && p2.subst(a,b) == newpol("0") ){
	        var sss = " -- points " + a.toUpperCase + " and " + b.toUpperCase + " must be different"
	        if(!restrictions.contains(sss))
	          restrictions.update(sss,1)
	        
	        p1 = p1.decomposeByFactor(a,b)
	        p2 = p2.decomposeByFactor(a,b)
	      }
	    for((a,b) <- l)
	      while(p1.subst(a,newpol("-1 * " + b), newpol("1"))._1 == newpol("0") && 
	          p2.subst(a,newpol("-1 * " + b), newpol("1"))._1 == newpol("0") ){
	        var sss = " -- points " + a.toUpperCase + " and " + b.toUpperCase + " can not be symmetric with respect to the center of the arbitrary circle "
	        if(!restrictions.contains(sss))
	          restrictions.update(sss,1)
	          
	        p1 = p1.decomposeBySum(a,b)
	        p2 = p2.decomposeBySum(a,b)
	      }
	    
	    var negative = true
	    for(factor <- p2.factors)
	      if(factor.c.u * factor.c.d > 0)
	        negative = false
	    if(negative){
	    	p1 = -p1
	    	p2 = -p2
	    }
	    
	    simplifyFractionByConstants(p1, p2)
	  }  
 
	  def simplifyPair(a : (Pol,Pol)) : (Pol,Pol) = {
	    simplifyFraction(a._1,a._2)
	  }
  //system with 2 equations as polynomials and unknown values a and a__. Returs a solution a = pu/pd

	  //the equations must be of degree 1 in both a and a__
	  //a__ * f1 + f2 = 0 
	  //a__ * s1 + s2 = 0
	  // ==> f2 * s1 - s2 * f1 = 0  
	def secondDegreeSystem(a : String, a__ : String, p1 : Pol , p2 : Pol) : (Pol,Pol) ={
  	  var f = p1.firstDegreeDecomp(a__)
  	  var s = p2.firstDegreeDecomp(a__)
  	  var newp = f._2 * s._1 - (f._1 * s._2)
  	  newp = simplifyFraction(newp, newpol("0"))._1
  	  var r = newp.firstDegreeDecomp(a)
  	  simplifyFraction(- r._2, (r._1))
  	}	  

	//one eq : a__ * f1 + f2 = 0 
	//returns (-f2, f1) simplified
	def firstDegreeSystem(a__ : String, p1 : Pol) : (Pol,Pol) ={
  	  var r = p1.firstDegreeDecomp(a__)
  	  simplifyFraction(r._2, -(r._1))
  	}	  
	
} 