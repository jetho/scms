
/** The environment abstraction which is used for implementing lexical scoping.*/


package org.jetho.scms


import scalaz._
import Scalaz._
import scala.collection.mutable.Map


/** An environment consists of a mapping from identifiers to values (expressions).
    It holds a reference to its parent environment to support nested scopes.*/
class Environment(ids: Map[String, Exp], parent: Option[Environment]) {

  type Scope = Map[String, Exp]
    
  /** Searching up the scope stack for a given identifier.
      Call the action function with the scope which contains the identifier.
      Fail if the scope stack doesn't contain a valid binding for the given identifier.*/
  private def forScopeOf(id: String)(action: (Scope => Exp)): Validation[String, Exp] =
    if (ids contains id)
      action(ids).success
    else
      parent.map(_.forScopeOf(id)(action)).getOrElse(("No valid binding found for Identifier " + id).fail)

  /** Search the scope stack for the given identifier and return the connected value.*/
  def lookup(name: String) =
    forScopeOf(name) { scope => 
      scope(name) 
    }

  /** Search the scope stack for the given identifier and update its value.
      Return the new value.*/
  def update(name: String, value: Exp) = 
    forScopeOf(name) { scope =>
      scope(name) = value
      value
    }

  /** Extend the environment by creating a new one with the current env as parent.*/
  def extend(bindings: List[(String, Exp)]) = new Environment(Map(bindings : _*), Some(this))
      
}


/** The empty environment represents the initial or top level environment.*/
object EmptyEnvironment extends Environment(Map[String, Exp](), None)



