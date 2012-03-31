
package org.jetho.scms


import org.specs2.mutable._
import scalaz._
import Scalaz._



class ReaderSpec extends Specification {

  "The Reader" should {
    "parse a number" in {
      Reader("23") must beEqualTo(NumExp(23).success)
    }
    "parse a string" in {
      Reader("\"Test\"") must beEqualTo(StringExp("Test").success)
    }
    "parse a symbol" in {
      Reader("cons") must beEqualTo(SymbolExp("cons").success)
    }
    "parse a boolean true" in {
      Reader("#t") must beEqualTo(BoolExp(true).success)
    }
    "parse a boolean false" in {
      Reader("#f") must beEqualTo(BoolExp(false).success)
    }
    "parse a quoted list" in {
      Reader("'(1 2 3)") must beEqualTo(ListExp(List(SymbolExp("quote"), ListExp(List(NumExp(1), NumExp(2), NumExp(3))))).success)
    }
    "parse a list" in {
      Reader("(+ 2 3)") must beEqualTo(ListExp(List(SymbolExp("+"), NumExp(2), NumExp(3))).success)
    }
    "parse a nested list" in {
      Reader("(cons 1 (cons 2 3))") must beEqualTo(ListExp(List(SymbolExp("cons"), NumExp(1), ListExp(List(SymbolExp("cons"), NumExp(2), NumExp(3))))).success)
    } 
    "parse a combined expression" in {
      Reader("(if (= x 1) (+ x y) (+ x z))") must beEqualTo(ListExp(List(SymbolExp("if"),
                                                                         ListExp(List(SymbolExp("="), SymbolExp("x"), NumExp(1))),
                                                                         ListExp(List(SymbolExp("+"), SymbolExp("x"), SymbolExp("y"))),
                                                                         ListExp(List(SymbolExp("+"), SymbolExp("x"), SymbolExp("z"))))).success)
    }              
    "fail for invalid expressions" in {
      Reader("(if (= x 1) (+ x y) (+ x y)") must beAnInstanceOf[Failure[String, Exp]]
    }
    
  }
  
}
