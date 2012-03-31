
package org.jetho.scms


import scalaz._
import Scalaz._
import scala.collection.mutable.Map


class Environment(ids: Map[String, Exp], parent: Option[Environment]) {

  type Scope = Map[String, Exp]
    
  private def forScopeOf(id: String)(action: (Scope => Exp)): Validation[String, Exp] =
    if (ids contains id)
      action(ids).success
    else
      parent.map(_.forScopeOf(id)(action)).getOrElse(("No valid binding found for Identifier " + id).fail)

  def lookup(name: String) =
    forScopeOf(name) { scope => 
      scope(name) 
    }

  def update(name: String, value: Exp) = 
    forScopeOf(name) { scope =>
      scope(name) = value
      value
    }

  def extend(bindings: List[(String, Exp)]) = new Environment(Map(bindings : _*), Some(this))
      
}


object EmptyEnvironment extends Environment(Map[String, Exp](), None)



