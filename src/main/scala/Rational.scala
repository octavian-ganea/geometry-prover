import java.util.StringTokenizer
import java.text.{NumberFormat, ParsePosition}

object Rational{

	def isNumeric(input: String): Boolean = {
	    val formatter = NumberFormat.getInstance
	    val pos = new ParsePosition(0)
	    formatter.parse(input, pos)
	    input.length == pos.getIndex // valid if parse position is at end of string
	}

	def cmmdc(xx : Int, yy : Int) : Int = {
	  var x = xx
	  var y = yy
	  if(x < 0)
	    x = -x
	  if(y < 0)
	    y = -y
	  if(y == 0)
	    return x
	  if(x == 0)
	    return y
	  if(x == 1 || y == 1)
	    return 1
	  if(x < y)
	    return cmmdc(x, y %x)
	  return cmmdc(x %y, y)
	}
	
	
	def newrational(s : String) = {
	  var tk = new StringTokenizer(s, " /")
	  var m = Nil : List[String]
	  assert(tk.hasMoreTokens, "[ERROR] bad defined rational number in " + s)
	  var u = Integer.parseInt(tk.nextToken)
	  var d = 1
	  if(tk.hasMoreTokens)
	    d = Integer.parseInt(tk.nextToken)
  	  assert(!tk.hasMoreTokens, "[ERROR] bad defined rational number in " + s)  
  	  var r = new Rational(u,d)
	  r.simplify
	}
	//====================== RATIONAL NUMBERS ====================================

class Rational(uuu : Int, ddd : Int){
	def this(u : Int) = this(u,1)
	def d : Int = ddd
	def u : Int = uuu
	
    override def toString :String = {
      var t = simplify 
      if( t.d != 1 )
    	  return "" + t.u + "/" + t.d
      return t.u.toString
    }
	
    def simplify : Rational = {
    	assert(d != 0, "[ERROR] rational number with 0 numitor")
    	var uu = u
    	var dd = d
    	var minus = (if(uu*dd >= 0) 0 else 1)
    	if ( uu < 0) uu = -uu
    	if ( dd < 0) dd = -dd
    	
	    var i = 2 : Int
	    while( i <= dd){
	      if(uu % i == 0 && dd %i == 0){
	        uu = uu/i
	        dd = dd/i
	        i = i - 1
	      }  
	      i = i + 1
	    }
    	if(minus == 1)
    	  uu = -uu
    	if(uu == 0)
    	  dd = 1
    	new Rational(uu,dd)
    }

    def *(that: Rational): Rational = {
  	  var r = new Rational(this.u * that.u, this.d * that.d )
  	  r.simplify
  	}
  	def /(that: Rational): Rational = {
  	  var r = new Rational(this.u * that.d, this.d * that.u )
  	  r.simplify
  	}  	
  	def +(that: Rational): Rational = {
  	  var r = new Rational(this.u * that.d + this.d * that.u, this.d * that.d )
  	  r.simplify
  	}
  	def -(that: Rational): Rational = {
  	  var r = new Rational(this.u * that.d - this.d * that.u, this.d * that.d )
  	  r.simplify
  	}
  	def ==(that: Rational): Boolean = {
  	  var m = this.simplify
  	  var n = that.simplify
  	  if(m.d == n.d && m.u == n.u)
  		  return true
  	  return false
  	}
  	def !=(that: Rational): Boolean = (!(this == that))
    def unary_- : Rational = {
      var x = new Rational(-u, d)
      x.simplify
    }
  	def *(that: Int): Rational = this * new Rational(that, 1)
  	def /(that: Int): Rational = this / new Rational(that, 1)
  	def +(that: Int): Rational = this + new Rational(that, 1)
  	def -(that: Int): Rational = this - new Rational(that, 1)
  	def ==(that: Int): Boolean = this == new Rational(that, 1)
  	def !=(that: Int): Boolean = (!(this == new Rational(that, 1)))

  }
}