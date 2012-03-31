
 /** The S-Expression Parser.*/


package org.jetho.scms


import scala.util.parsing.combinator.RegexParsers
import scalaz._
import Scalaz._



object Reader extends RegexParsers {

  override val skipWhitespace = false

  def space = rep1(" ")

  def symbol: Parser[Exp] = """[a-zA-Z!#$%&|*+/:<=>?@^_~-]\w*""".r ^^ { 
    case "#t" => BoolExp(true)
    case "#f" => BoolExp(false)
    case sym => SymbolExp(sym)
  }

  def string: Parser[StringExp] = "\"" ~> rep("[^\"]".r) <~ "\"" ^^ { s => StringExp(s.mkString) }

  def number: Parser[NumExp] = rep1("[0-9]+".r) ^^ { n => NumExp(n.mkString.toInt) }

  def list: Parser[ListExp] = "(" ~> repsep(exp, space) <~ ")" ^^ { l => ListExp(l) }

  def quoted: Parser[ListExp] = "'" ~> exp ^^ { e => ListExp(List(SymbolExp("quote"), e)) }

  def exp: Parser[Exp] = list | symbol | string | number | quoted  


  def apply(input: String): Validation[String, Exp] = 
    parse(exp, input) match {
      case Success(res, _) => res.success
      case NoSuccess(msg, _) => msg.fail 
    }
  
}
  
