import scala.util._
import scala.io._

object Definitions {
  import Polinomial._
  
  sealed abstract class Tree 
  sealed abstract class Constraint extends Tree{
    def equa : Pol 
  }
  sealed abstract class EquationHypSingle extends Tree{
    def value : (Pol, Pol)
  }
  sealed abstract class EquationHypDouble extends Tree{
    def equa(x : Point) : Pol
  }
  sealed abstract class EquationConcl extends Tree{
    def equa : Pol
  }
  sealed abstract class DeterministicPoint extends Tree


  case class Problem(description: Description, hypothesis: Hypothesis, conclusion: Conclusion) extends Tree {
    override def toString: String = {
      description + "\n\n" + hypothesis + "\n" + conclusion
    }
  }

  case class Description(d: String) extends Tree {
    override def toString: String = d
  }

  case class Hypothesis(arbitrary: Arbitrary, semiarbitrary : Semiarbitrary, deterministic : Deterministic , constraint : Constraint) extends Tree {
    override def toString: String = {
      var s = "Hypothesis:\n\n" + arbitrary + semiarbitrary + deterministic 
      if(constraint != null)
    	s + constraint
      else
        s
    }
  }

  case class Arbitrary(arbitrary_circle : ArbitraryCircle, arbitrary_point_list : List[ArbitraryPoint]) extends Tree {
    override def toString: String = {
      var s = arbitrary_circle.toString
      for(e <- arbitrary_point_list)
        s = s + e
      s + "\n"
    }
  }
  
  //a circle of center and radix "radix"
  case class ArbitraryCircle(circle_name : CircleName, center : Point) extends Tree {
    override def toString: String = {
      "Let an arbitrary circle " + circle_name + "(" + center + ",radix)\n"
    }
  }

  case class ArbitraryPoint(p : Point) extends Tree {
    override def toString: String = {
      "Let an arbitrary point " + p + "\n"
    }
  }
  
  case class Semiarbitrary(semiarbitrary_point_list: List[SemiarbitraryPoint], euler_circle : EulerCircle) extends Tree{
    override def toString: String = {
      var s = ""
      for(e <- semiarbitrary_point_list)
        s = s + e
      if(euler_circle != null)
        s + euler_circle + "\n"
      else
        s + "\n"
    }
  }
  
  case class SemiarbitraryPoint(p : Point, eq_double : EquationHypDouble) extends Tree{
    override def toString: String = {
      "Let a semiarbitrary point " + p + " situated on " + eq_double + "\n"
    }
  }

  case class EulerCircle(p1 : Point, p2 : Point, p3 : Point, circle : CircleName) extends Tree{
    override def toString: String = {
      "Let the Euler circle with respect to triangle " + p1 + p2 + p3 + " be " + circle + "(Omega, radix_E)\n"
    }
  }
  
  case class Deterministic(list : List[DeterministicPoint]) extends Tree{
    override def toString: String = {
      var s = ""
      for(e <- list)
        s = s + e
      s 
    } 
  }
    
  case class DeterministicPointSingle(p : Point, eq_single : EquationHypSingle) extends DeterministicPoint {
    override def toString: String = "Let a 1-equation deterministic point " + p + " situated on " + eq_single + "\n"
  }

  case class DeterministicPointDouble(p : Point, eq1 : EquationHypDouble, eq2 : EquationHypDouble) extends DeterministicPoint {
    override def toString: String = "Let a 2-equations deterministic point " + p + " situated on " + eq1 + " and " + eq2 + "\n"
  }
  
  case class Conclusion(eq_concl : EquationConcl) extends Tree {
    override def toString: String = "Conclusion: " + eq_concl
  }

  // ======== Constraints ============
  case class ConstrCollinear(p1 : Point, p2 : Point, p3 : Point) extends Constraint{ 
    override def toString: String = "Constraint: points " + p1 + " " + p2 + " " + p3 + " are collinear\n"
    def equa : Pol = {
      var a = p1.afix
      var a__ = p1.conjugate
      var b = p2.afix
      var b__ = p2.conjugate
      var c = p3.afix
      var c__ = p3.conjugate
      var q1 = (singlepol(a) - singlepol(b)) * (singlepol(b__) - singlepol(c__))
      var q2 = (singlepol(b) - singlepol(c)) * (singlepol(a__) - singlepol(b__))
      q1 - q2
    }
  }
  case class ConstrConcurrent(l1 : Line, l2 : Line, l3 : Line) extends Constraint{
    override def toString: String = "Constraint: lines " + l1 + " " + l2 + " " + l3 + " are concurrent\n"     
    def equa : Pol = {
      null
    }
  }
  case class ConstrPerpendicular(l1 : Line, l2 : Line) extends Constraint{
    override def toString: String = "Constraint: lines " + l1 + " " + l2  + " are perpendicular\n"       
    def equa : Pol = {
      var a = l1.p1.afix
      var a__ = l1.p1.conjugate
      var b = l1.p2.afix
      var b__ = l1.p2.conjugate
      var c = l2.p1.afix
      var c__ = l2.p1.conjugate
      var d = l2.p2.afix
      var d__ = l2.p2.conjugate
      var q1 = (singlepol(a) - singlepol(b)) * (singlepol(d__) - singlepol(c__))
      var q2 = (singlepol(d) - singlepol(c)) * (singlepol(a__) - singlepol(b__))
      q1 + q2
    }    
  }
  case class ConstrParallel(l1 : Line, l2 : Line) extends Constraint{
    override def toString: String = "Constraint: lines " + l1 + " " + l2  + " are parallel\n"      
    def equa : Pol = {
      var a = l1.p1.afix
      var a__ = l1.p1.conjugate
      var b = l1.p2.afix
      var b__ = l1.p2.conjugate
      var c = l2.p1.afix
      var c__ = l2.p1.conjugate
      var d = l2.p2.afix
      var d__ = l2.p2.conjugate
      var q1 = (singlepol(a) - singlepol(b)) * (singlepol(d__) - singlepol(c__))
      var q2 = (singlepol(d) - singlepol(c)) * (singlepol(a__) - singlepol(b__))
      q1 - q2
    }     
  }
  // =================================
  
  // ======== Equations hypothesis single ============
  case class EqCentroid(p1 : Point, p2 : Point, p3 : Point) extends EquationHypSingle{
    override def toString: String = "centroid of triangle " + p1 + " " + p2 + " " + p3  
    def value : (Pol, Pol) = {
      (singlepol(p1.afix) + singlepol(p2.afix) + singlepol(p3.afix), newpol("3"))
    }     
  }
  case class EqProjectionPointLine(p1 : Point, l1 : Line) extends EquationHypSingle{
    override def toString: String = "projection of " + p1 + " on " + l1  
    def value : (Pol , Pol) = { 
      var xx = new Point("X55")
      var x = xx.afix
      var x__ = xx.conjugate
      secondDegreeSystem(x, x__, new EqLine(l1).equa(xx) , new EqPerpendicular(new Line(xx,p1) ,l1).equa(xx))
    }
  }
  case class EqSymPointPoint(p1 : Point, p2 : Point) extends EquationHypSingle{
    override def toString: String = "symmetric point of " + p1 + " with respect to " + p2       
    def value : (Pol, Pol) = { 
      var p = - singlepol(p1.afix)
      p = p + newpol("2 * " + p2.afix) 
      (p, newpol("1"))
    }     
  }
  case class EqMidpoint(p1 : Point, p2 : Point) extends EquationHypSingle{
    override def toString: String = "midpoint of " + p1 + " " + p2       
    def value : (Pol, Pol) = {
      (singlepol(p1.afix) + singlepol(p2.afix), newpol("2"))
    }     
  }  
  // =================================
 
  // ======== Equations hypothesis double ============
  case class EqLine(l : Line) extends EquationHypDouble{
    override def toString: String = l.toString       
    def equa(x : Point) : Pol = {
	  new ConstrCollinear(x, l.p1, l.p2 ).equa
    }
  }
  case class EqCircle(circle : CircleName) extends EquationHypDouble{
    override def toString: String = "circle " + circle       
    def equa(x : Point) : Pol = {
      var c = x.afix
      var c__ = x.conjugate
      if(circle.name.compareTo("Euler") == 0){
        assert(false, "Euler circle not supported yet")
      }
      return singlepol(c) * singlepol(c__) - newpol("1") 
    }    
  }
  case class EqPerpendicular(l1 : Line, l2 : Line) extends EquationHypDouble{
    override def toString: String = l1 + " perpendicular on " + l2      
    def equa(x : Point) : Pol = {
  	  new ConstrPerpendicular(l1, l2).equa
    }        
  }
  case class EqParallel(l1 : Line, l2 : Line) extends EquationHypDouble{
    override def toString: String = l1 + " parallel with " + l2      
    def equa(x : Point) : Pol = {
	  new ConstrParallel(l1,l2).equa
    }        
  }
  case class EqPerpBisector(l : Line) extends EquationHypDouble{
    override def toString: String = "perpendicular bisector of " + l 
    def equa(x : Point) : Pol = {
      var a = l.p1.afix
      var a__ = l.p1.conjugate
      var b = l.p2.afix
      var b__ = l.p2.conjugate
      var c = x.afix
      var c__ = x.conjugate
      var q1 = (singlepol(c) - (newpol("1/2 * " + a) + newpol("1/2 * " + b) )) * (singlepol(a__) - singlepol(b__))
      var q2 = (singlepol(a) - singlepol(b)) * (singlepol(c__) - (newpol("1/2 * " + a__) + newpol("1/2 * " + b__) ))
      q1 + q2
    }        
  }
  case class EqTangent(circle : CircleName, p : Point) extends EquationHypDouble{
    override def toString: String = "tangent at circle " + circle + " in point " + p 
    def equa(x : Point) : Pol = {
      var a = p.afix
      var a__ = p.conjugate
      var c = x.afix
      var c__ = x.conjugate
      if(circle.name.compareTo("Euler") == 0){
        assert(false, "Euler circle not supported yet")
      }
      return singlepol(c) + (singlepol(c__) * singlepol(a)* singlepol(a)) - newpol("2 * " + a) 
    }       
  }
  // =================================						  
						  
  // ======== Equations conclusion ============
  case class ConclConcyclic(p1 : Point, p2 : Point, p3 : Point, p4 : Point) extends EquationConcl{
    override def toString: String = "points " + p1 + ", " + p2 + ", " + p3 + " and " + p4 + " are concyclic\n"   
    def equa : Pol = {
      var a = p1.afix
      var a__ = p1.conjugate
      var b = p2.afix
      var b__ = p2.conjugate
      var c = p3.afix
      var c__ = p3.conjugate
      var d = p4.afix
      var d__ = p4.conjugate
      var q1 = (singlepol(c) - singlepol(b)) * (singlepol(a) - singlepol(d)) * (singlepol(a__) - singlepol(b__)) * (singlepol(c__) - singlepol(d__))  
      var q2 = (singlepol(a) - singlepol(b)) * (singlepol(c) - singlepol(d)) * (singlepol(c__) - singlepol(b__)) * (singlepol(a__) - singlepol(d__))
      q1 - q2
    }    
  }
  case class ConclCollinear(p1 : Point, p2 : Point, p3 : Point) extends EquationConcl{
    override def toString: String = "points " + p1 + ", " + p2 + " and " + p3 + " are collinear\n"      
    def equa : Pol = {
      new ConstrCollinear(p1,p2,p3).equa
    }
  }
  case class ConclConcurrent(l1 : Line, l2 : Line, l3 : Line) extends EquationConcl{
    override def toString: String = l1 + ", " + l2 + " and " + l3 + " are concurrent\n"
    def equa : Pol = {
	  null
    }
  }
  case class ConclPerpendicular(l1 : Line, l2 : Line) extends EquationConcl{
    override def toString: String = l1 + " and " + l2  + " are perpendicular\n"  
    def equa : Pol = {
      new ConstrPerpendicular(l1,l2).equa
    }    
  }
  case class ConclParallel(l1 : Line, l2 : Line) extends EquationConcl{
    override def toString: String = l1 + " and " + l2  + " are parallel\n"      
    def equa : Pol = {
      new ConstrParallel(l1,l2).equa
    }    
  }
  case class ConclEquilateral(p1 : Point, p2 : Point, p3 : Point) extends EquationConcl{
    override def toString: String = "triangle " + p1 + " " + p2 + " " + p3 + " is equilateral \n"      
    def equa : Pol = {
      var a = p1.afix
      var b = p2.afix
      var c = p3.afix
      (singlepol(a) * singlepol(a)) + (singlepol(b) * singlepol(b)) + (singlepol(c) * singlepol(c)) -
      (singlepol(a) * singlepol(b)) + (singlepol(b) * singlepol(c)) + (singlepol(c) * singlepol(a)) 
    }    
    
  }  
  // =================================
  
  case class Point(p: String) extends Tree{
    override def toString: String = p
    def afix : String = p.toLowerCase
    def conjugate : String = this.afix + "__"
  }
  case class Line(p1: Point, p2 : Point) extends Tree{
    override def toString: String = "line " + p1 + "-" + p2
  }

  //circle_name = "Circ", "Inscr", "Euler"
  case class CircleName(name : String) extends Tree{
    override def toString: String = name
  }
}
