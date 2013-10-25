import java.io.PrintStream
import java.io.FileOutputStream
import scala.util._
import scala.io._
import scala.collection.mutable.ListMap

object Main {
	import Definitions._
	import Parser._
	import Polinomial._
	import Rational._
	import Factor._
	import ProblemSolver._
	import SyntacticSugar._
	
    var out = null : PrintStream 
    
	def append(s: String) : Unit = {
	  println(s)
	  out.println(s)
	}
	
	def main(args : Array[String]) : Unit = {
		assert(args.length == 1, "One single argument - the name of the file - needed")
		
		var file = args(0) + ".out"
		while(file.indexOf("/") >= 0)
		  file = file.substring(file.indexOf("/") + 1)
		file = "tests/output/" + file 
		out = new PrintStream(new FileOutputStream(file))
	
		val reporter = new DefaultReporter
		append("====== Parsing .... ")
		
		var problem = parseFile(args(0)) //syntactic tree of the program 
		problem = syntacticSugar(problem)
		append(problem.toString)
		append("====== Parse OK \n")

		append("====== Proof of the problem:\n")
		generateEquations(problem)
		append("=========== Equations were generated successfully ========\n")

		if(Polinomial.restrictions.size > 0){
			append("======= The conclusion may be correct only unde the following restrictions:")
			for(e <- Polinomial.restrictions.toList)
			  append(e._1)
		}
		
		append("\n====== Trying to solve the goal of the problem ...")
		solveProblem
		
		
	}

}

