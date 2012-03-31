
package org.jetho.scms


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
      StringExp("test").toString must beEqualTo("test")
    }
    "be the concatenation of the elements if it's a list " in {
      ListExp(List(SymbolExp("cons"), NumExp(1), ListExp(List(SymbolExp("cons"), NumExp(2), NumExp(3))))).toString must beEqualTo("(cons 1 (cons 2 3))")
    }
  }
  
}
