
package org.jetho.scms

import scalaz._
import Scalaz._
import org.specs2.mutable._



class Exp2StringSpec extends Specification {

  "The string representation of an expression" should {
    "be '#t' for a boolean true" in {
      BoolExp(true).toString must beEqualTo("#t")
    }
    "be '#f' for a boolean false" in {
      BoolExp(false).toString must beEqualTo("#f")
    }
    "be the symbol's name for a symbol" in {
      SymbolExp("x").toString must beEqualTo("x")
    }
    "be the numerical value if it's a number" in {
      NumExp(5).toString must beEqualTo("5")
    }
    "be a string if it contains a string" in {
      StringExp("test").toString must beEqualTo("\"test\"")
    }
    "be the concatenation of the elements if it's a list " in {
      ListExp(List(SymbolExp("cons"), NumExp(1), ListExp(List(SymbolExp("cons"), NumExp(2), NumExp(3))))).toString must beEqualTo("(cons 1 (cons 2 3))")
    }
    "be display dot notation if it's a dotted list" in {
      DottedListExp(List(SymbolExp("+"), NumExp(1)), ListExp(List(NumExp(2)))).toString must beEqualTo("(+ 1 . (2))")
    }
    "be <primitive> for a primitive function" in {
      PrimitiveFunc(_ => Default("dummy").left).toString must beEqualTo("<primitive>")
    }
    "be <IO primitive> for a primitive IO function" in {
      IOFunc(_ => Default("dummy").left).toString must beEqualTo("<IO primitive>")
    }
    "be <IO Port> for input and output channels" in {
      InPort(null).toString must beEqualTo("<IO port>")
      OutPort(null).toString must beEqualTo("<IO port>")
    }
    "be the function header for lambdas" in {
      Func(List("x", "y"), None, Nil, EmptyEnvironment).toString must beEqualTo("(lambda (x y) ...)")
      Func(List("x", "y"), Some("z"), Nil, EmptyEnvironment).toString must beEqualTo("(lambda (x y . z) ...)")
    }
  }
  
}
