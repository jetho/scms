
/** The environment abstraction used for nested scoping.*/


package org.jetho.scms


import scalaz._
import Scalaz._
import scala.collection.mutable.Map


/** An environment represents a mapping from identifiers to values (bindings).
    It holds a reference to its parent environment to support nested scopes.*/
class Environment(ids: Map[String, Exp], parent: Option[Environment]) {

  type Scope = Map[String, Exp]
    
  /** Search up the scope stack for a given identifier.
      Apply the action function to the scope which contains the identifier.
      Fail if the scope stack doesn't contain a valid binding for the identifier.*/
  private def forScopeOf(id: String)(action: (Scope => Exp)): ErrorMsg \/ Exp =
    if (ids contains id)
      action(ids).right
    else
      parent.map(_.forScopeOf(id)(action)).getOrElse(UnboundVar("Unbound Variable", id).left)

  /** Search the scope stack for the given identifier and return the associated value.*/
  def lookup(name: String) =
    forScopeOf(name) { scope => 
      scope(name) 
    }

  /** Search the scope stack for the given identifier and update its value.
      Return the new value.*/
  def update(name: String)(value: Exp) = 
    forScopeOf(name) { scope =>
      scope(name) = value
      value
    }

  def define(name: String)(value: Exp) = {
    ids(name) = value
    value.right
  }

  /** Extend the environment by creating a new one with the current env as parent.*/
  def extend(bindings: List[(String, Exp)]) = new Environment(Map(bindings : _*), Some(this))
      
}


/** The empty environment represents the initial or top level environment.*/
object EmptyEnvironment extends Environment(Map[String, Exp](), None)



