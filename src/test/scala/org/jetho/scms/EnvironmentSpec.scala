
package org.jetho.scms


import org.specs2.mutable._
import scalaz._
import Scalaz._
import scala.collection.mutable.Map



class EnvironmentSpec extends Specification {

  "The Empty Environment" should {
    "contain no bindings" in {
      EmptyEnvironment.lookup("x") must beEqualTo("No valid binding found for Identifier x".fail)
    }
    "not be updateable" in {
      EmptyEnvironment.update("x", NumExp(5)) must beEqualTo("No valid binding found for Identifier x".fail)
    }
    "be extendable" in {
      val env = EmptyEnvironment.extend(List(("x", NumExp(5)), ("s", StringExp("Test"))))
      env must beAnInstanceOf[Environment]
    }
  }

  "An Environment with several bindings" should {
    "return the right value for a valid binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.lookup("x") must beEqualTo(NumExp(5).success)
    }
    "return an error for an invalid binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.lookup("z") must beEqualTo("No valid binding found for Identifier z".fail)
    }      
    "be updateable" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.update("x", NumExp(8))
      env.lookup("x") must beEqualTo(NumExp(8).success)
    }
    "be extendable" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("z", NumExp(9))))
      extendedEnv must beAnInstanceOf[Environment]
    }
  }

  "An extended Environment with several lexical levels" should {
    "prefer bindings that shadow outer bindings" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.lookup("x") must beEqualTo(NumExp(8).success)
    }
    "search up the lexical scopes for valid bindings" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      env.lookup("s") must beEqualTo(StringExp("Test").success)
    }
    "return an error if a binding isn't defined in the scope stack" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      env.lookup("z") must beEqualTo("No valid binding found for Identifier z".fail)
    }
    "update only the local binding if the local binding shadows an outer binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.update("x", NumExp(99))
      extendedEnv.lookup("x") must beEqualTo(NumExp(99).success)
      env.lookup("x") must beEqualTo(NumExp(5).success)
    }      
    "update the binding in an enclosing scope if the binding isn't shadowed by a more local binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.update("s", StringExp("Test2"))
      extendedEnv.lookup("s") must beEqualTo(StringExp("Test2").success)
      env.lookup("s") must beEqualTo(StringExp("Test2").success)
    }      
  }
  
}
