
//////////////////
// 1. Implicits //
//////////////////

// a) conversion
implicit def double2Int(d: Double) = d.toInt    //> double2Int: (d: Double)Int
val x: Int = 44.0                              

implicit def defaultVal: Double = 42.0          //> defaultVal: => Double
//implicit def otherVal: Double = 40.0

// b) implicit parameter
def foo(implicit x: Double) = println(x)        //> foo: (implicit x: Double)Unit
foo                                            

// c) adding a new method to existing class (!)
object Helpers {
  implicit class StringUtils(val s: String) {
    def increment = s.map(c => (c + 1).toChar)
  }
}

import Helpers._
"HAL".increment                                 

"1".increment                                   

//////////////////////////
// 2. Partial functions //
//////////////////////////

val root: PartialFunction[Double, Double] = {
  case d if (d >= 0) => math.sqrt(d)
}                                              

root isDefinedAt -1                            

// collect applies a partial function to all list elements for which it is defined, and creates a new list
List(-2.0, 0, 2, 4) collect root                

// Documentation: http://www.scala-lang.org/api/current/index.html#scala.PartialFunction

