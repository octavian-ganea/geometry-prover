import java.lang.StringBuffer
import java.io._
import scala.collection.mutable.ListMap
import java.util.StringTokenizer

object Factor{
  import Rational._
  
  def isConjugate(s : String) : Boolean = {
    if(s.length < 2){
      return false
    }
    if(s.substring(s.length - 2).compareTo("__") == 0){
      return true
    }
    return false
  }
  def isConjugateOfPoint(z : String, z__ : String) : Boolean = {   
    if(isConjugate(z__) && z__.substring(0, z__.length - 2).compareTo(z) == 0)
      return true
    return false
  }

  def listToFactor(l: List[String]): Factor = {
    var res = new Factor(new Rational(1), new ListMap)
    for (s <- l) {
      var x = new ListMap[String, Int]
      x.update(s, 1)
      res = res * new Factor(new Rational(1), x)
    }
    res
  }


   //string must contain a constant in front of it, even if it is 1
  def newfactor(str : String) : Factor = {
	  var tk = new StringTokenizer(str, "*")
      assert(tk.hasMoreTokens, "[ERROR] factor not well defined in " + str)
      var cc = newrational(tk.nextToken)
      var map = new ListMap[String, Int]
      while(tk.hasMoreTokens){
    	  var s = tk.nextToken
    	  var ww = new StringTokenizer(s, " ^")
    	  assert(ww.hasMoreTokens, "[ERROR] factor not well defined in " + s)
    	  var i = 1
    	  var point = ww.nextToken
    	  if(ww.hasMoreTokens)
    	    i = Integer.parseInt(ww.nextToken)
    	  assert(!ww.hasMoreTokens, "[ERROR] factor not well defined in " + s)
    	  if(i > 0)
	    	  if(map.contains(point))  
	    		map.update(point,i + map.get(point).get)
	    	  else
	    	    map.update(point,i)
      }
      var list = new ListMap[String,Int] 
      list = list ++ map.toList.sortWith((e1,e2) => {e1._1.compareTo(e2._1) > 0})    
      new Factor(cc, list).simplify
  }
  //================================ FACTORS ===================================
  
  abstract class Formula
  case class Factor(val ccc: Rational, val listlist: ListMap[String, Int]) extends Formula {
    def c : Rational = ccc
    def list : ListMap[String, Int] = listlist
     
    def simplify : Factor ={
      var l = new ListMap[String,Int]
      if(c == 0)
        return new Factor(c, l)
      l = l ++ list.toList.sortWith((e1,e2) => {e1._1.compareTo(e2._1) > 0})
      for ( e <- l)
        if(e._2 == 0)
          l = l - e._1
      new Factor(c,l)
    }

    def degree: Int = {
      var deg = 0
      for( e <- list.toList){
        if(! isConjugate(e._1)){
	      deg = deg + this.degreeOfVar(e._1)
        }
        else{
          deg = deg - this.degreeOfVar(e._1)
        }
      }
      deg
    }
    
    //without conjugate degree:
    def degreeOfVar(z: String): Int = {
      if(!list.contains(z))
        return 0
      return list.get(z).get
    }
    
    //f = f / z^n
    def divide(z : String, n : Int) : Factor = {
      assert(degreeOfVar(z) >= n, "[ERROR] Factor.divide was called with bigger n: " + n + " z:" +z + " deg:" + degreeOfVar(z) )
      var l = list.clone
      l.update(z, degreeOfVar(z) - n)
      (new Factor(c,l)).simplify
    }
    
    def containsVar(z: String): Boolean = list.contains(z)
    
    def *(f: Factor): Factor = {
      var reslist = list.clone
      for(e <- f.list){
        var plus = f.list.get(e._1).get
        if(list.contains(e._1))
          reslist.update(e._1, plus + list.get(e._1).get)
        else
          reslist.update(e._1, plus)
      }
      var res = new Factor(this.c * f.c, reslist)
      res.simplify
    }

    def ==(f: Factor): Boolean = {
      if(null == f)
	      return false
      if (this.c != f.c || this.list.size != f.list.size)
        return false
      for (e <- list.toList) {
        var x = -1 
        var plus = list.get(e._1)
        plus match {
          case Some(v) => x = v
          case _ =>
        }
        var y = -1
        var plus1 = f.list.get(e._1)
        plus1 match {
          case Some(v1) => y = v1 
          case _ =>
        }
        if(x != y)
          return false
      }
      return true
    }
    
    def !=(f : Factor): Boolean = !(this == f)
    
    def unary_- = new Factor(-c, list)

    def formulaWithoutOne(z : String) : Factor = {
      var l = list.clone
      if(l.contains(z))
        l = l - z
      new Factor(c, l)
    }

    override def toString : String = {
      var f = this.simplify
      var s = ""
      if(f.c == new Rational(1))
        s = ""
        else if(f.c == new Rational(-1))
        	s = " - "
        	else
        	  if(f.c.u < 0)
        	    s = " - " + new Rational(-f.c.u, f.c.d) + " * "
        	    else
        	    	s = f.c.toString + " * "
      for(e <- f.list.toList){
        if(e._2 > 1)
          s = s + e._1 + "^" + e._2 + " * "
        else
          s = s + e._1 + " * "
      }
      if(s.length > 3)
    	  s = s.substring(0, s.length - 3)
      else
    	  s = s + "1"
      s
    }
    
    def *(cc : Rational) : Factor = new Factor(c * cc, list)
        
    //substitute a with b
    def subst(a : String , b : String) : Factor = {
      var f = this
      var l = list.clone
      if(!l.contains(a))
        return f
      var i = l.get(a).get
      l.remove(a)
      if(l.contains(b))
        l.update(b,l.get(b).get + i)
      else
    	l.update(b,i)
      return new Factor(f.c ,l)
    }
    
    def conjugate : Factor = {
      var l = new ListMap[String, Int] 
      for((s,i) <- this.list.toList){
        if(s.length < 2 || s.substring(s.length - 2, s.length).compareTo("__") != 0)
          l.update(s + "__", i)
        else
          l.update(s.substring(0,s.length - 2), i)
      }
      new Factor(this.c, l)
    }
  }
}