import scala.util._
import scala.io._
import scala.collection.mutable.ListMap

object SyntacticSugar {
	import Definitions._
	
	def syntacticSugar( prob : Problem) : Problem = {
		var p = prob
	  
		prob.conclusion.eq_concl match {
		  case ConclConcurrent(l1,l2,l3) => {
		    var det = p.hypothesis.deterministic.list
		    var newpoint = new Point("X13")
		  	var eq = DeterministicPointDouble(newpoint, new EqLine(l1), new EqLine(l2))
		  	det = det ::: List(eq)
		  	var concl = new Conclusion(new ConclCollinear(newpoint, l3.p1, l3.p2))
		    var hyp = p.hypothesis
		    p = new Problem(p.description,new Hypothesis(hyp.arbitrary,hyp.semiarbitrary,new Deterministic(det),hyp.constraint),concl)
		  }
		  case _ =>
		}
		p
	}
}