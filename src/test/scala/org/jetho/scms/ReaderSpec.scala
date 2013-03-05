
package org.jetho.scms


import org.specs2.mutable._
import scalaz._
import Scalaz._


class ReaderSpec extends Specification {

  "The Reader" should {
    "parse numbers" in {
      Reader.readExpr("23") must beEqualTo(NumExp(23).right)
    }
    "parse strings" in {
      Reader.readExpr("\"Test\"") must beEqualTo(StringExp("Test").right)
    }
    "parse symbols" in {
      Reader.readExpr("cons") must beEqualTo(SymbolExp("cons").right)
    }
    "parse boolean true literals" in {
      Reader.readExpr("#t") must beEqualTo(BoolExp(true).right)
    }
    "parse boolean false literals" in {
      Reader.readExpr("#f") must beEqualTo(BoolExp(false).right)
    }
    "parse quoted expressions" in {
      Reader.readExpr("'(1 2 3)") must beEqualTo(ListExp(List(SymbolExp("quote"), ListExp(List(NumExp(1), NumExp(2), NumExp(3))))).right)
    }
    "parse lists" in {
      Reader.readExpr("(+ 2 3)") must beEqualTo(ListExp(List(SymbolExp("+"), NumExp(2), NumExp(3))).right)
    }
    "parse dotted lists" in {
      Reader.readExpr("(+ 1 . (2 3))") must beEqualTo(DottedListExp(List(SymbolExp("+"), NumExp(1)), ListExp(List(NumExp(2), NumExp(3)))).right)
    }
    "parse nested lists" in {
      Reader.readExpr("(cons 1 (cons 2 3))") must beEqualTo(ListExp(List(SymbolExp("cons"), NumExp(1), ListExp(List(SymbolExp("cons"), NumExp(2), NumExp(3))))).right)
    } 
    "parse combined expressions" in {
      Reader.readExpr("(if (= x 1) (+ x y) (+ x z))") must beEqualTo(ListExp(List(SymbolExp("if"),
                                                                         ListExp(List(SymbolExp("="), SymbolExp("x"), NumExp(1))),
                                                                         ListExp(List(SymbolExp("+"), SymbolExp("x"), SymbolExp("y"))),
                                                                         ListExp(List(SymbolExp("+"), SymbolExp("x"), SymbolExp("z"))))).right)
    }              
    "parse lists of expressions" in {
      Reader.readExprList("(cons 1 (cons 2 3)) (+ 1 2) (define a 1)") must beEqualTo(
        List(
          ListExp(List(SymbolExp("cons"), NumExp(1), ListExp(List(SymbolExp("cons"), NumExp(2), NumExp(3))))),
          ListExp(List(SymbolExp("+"), NumExp(1), NumExp(2))),
          ListExp(List(SymbolExp("define"), SymbolExp("a"), NumExp(1)))
        ).right)          
    } 
    "fail for invalid expressions" in {
      Reader.readExpr("(if (= x 1) (+ x y) (+ x y)") must beAnInstanceOf[scalaz.-\/[_]]
      Reader.readExprList("(+ 1 2) (if (= x 1) (+ x y) (+ x y) (define a 1)") must beAnInstanceOf[scalaz.-\/[_]]
      Reader.readExprList("(if (= x 1) (+ x y) (+ x y) (define a") must beAnInstanceOf[scalaz.-\/[_]]
    }
    
  }
}
