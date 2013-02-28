
package org.jetho.scms


import org.specs2.mutable._
import scalaz._
import Scalaz._
import scala.collection.mutable.Map



class EnvironmentSpec extends Specification {

  "The Empty Environment" should {
    "contain no bindings" in {
      EmptyEnvironment.lookup("x") must beEqualTo(UnboundVar("Unbound Variable", "x").left[Exp])
    }
    "not be updateable" in {
      EmptyEnvironment.update("x")(NumExp(5)) must beEqualTo(UnboundVar("Unbound Variable", "x").left[Exp])
    }
    "be extendable" in {
      val env = EmptyEnvironment.extend(List(("x", NumExp(5)), ("s", StringExp("Test"))))
      env must beAnInstanceOf[Environment]
    }
  }

  "An Environment with several bindings" should {
    "return the right value for a valid binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.lookup("x") must beEqualTo(NumExp(5).right)
    }
    "return an error for an invalid binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.lookup("z") must beEqualTo(UnboundVar("Unbound Variable", "z").left[Exp])
    }      
    "be updateable for valid bindings" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.update("x")(NumExp(8))
      env.lookup("x") must beEqualTo(NumExp(8).right)
    }
    "not be updateable for invalid bindings" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.update("z")(NumExp(6)) must beEqualTo(UnboundVar("Unbound Variable", "z").left[Exp])
    }
    "allow the definition of a new binding; the definition must return the value of the new binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.define("z")(NumExp(6)) must beEqualTo(NumExp(6).right)
      env.lookup("z") must beEqualTo(NumExp(6).right)
    }
    "ensure that the redefinition of a valid binding updates its value" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      env.define("x")(NumExp(6)) must beEqualTo(NumExp(6).right)
      env.lookup("x") must beEqualTo(NumExp(6).right)
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
      extendedEnv.lookup("x") must beEqualTo(NumExp(8).right)
    }
    "search up the lexical scopes for valid bindings" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      env.lookup("s") must beEqualTo(StringExp("Test").right)
    }
    "return an error if a binding isn't defined in the scope stack" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      env.lookup("z") must beEqualTo(UnboundVar("Unbound Variable", "z").left[Exp])
    }
    "update only the local binding if the local binding shadows an outer binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.update("x")(NumExp(99))
      extendedEnv.lookup("x") must beEqualTo(NumExp(99).right)
      env.lookup("x") must beEqualTo(NumExp(5).right)
    }      
    "update the binding in an enclosing scope if the binding isn't shadowed by a more local binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.update("s")(StringExp("Test2"))
      extendedEnv.lookup("s") must beEqualTo(StringExp("Test2").right)
      env.lookup("s") must beEqualTo(StringExp("Test2").right)
    }
    "only allow definitions of new bindings in the current lexical scope" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.define("z")(NumExp(6)) should beEqualTo(NumExp(6).right)
      extendedEnv.lookup("z") must beEqualTo(NumExp(6).right)
      env.lookup("z") must beEqualTo(UnboundVar("Unbound Variable", "z").left[Exp])
    }      
    "prevent that a definition of a new binding in the current scope affects the shadowed binding" in {
      val env = new Environment(Map("x" -> NumExp(5), "s" -> StringExp("Test")), None)
      val extendedEnv = env.extend(List(("x", NumExp(8))))
      extendedEnv.define("s")(StringExp("Test new")) should beEqualTo(StringExp("Test new").right)
      env.lookup("s") must beEqualTo(StringExp("Test").right)
    }      
  }
  
}
