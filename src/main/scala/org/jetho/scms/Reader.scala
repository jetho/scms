
 /** The S-Expression Parser.*/


package org.jetho.scms


import scala.util.parsing.combinator.RegexParsers
import scalaz._
import Scalaz._



object Reader extends RegexParsers {

  private def space  = "[ \\n]*".r

  private def symbol: Parser[Exp] = """[a-zA-Z!#$%&|*+/:<=>?@^_~-]\w*""".r ^^ { 
    case "#t" => BoolExp(true)
    case "#f" => BoolExp(false)
    case sym => SymbolExp(sym)
  }

  private def string: Parser[StringExp] = "\"" ~> rep("[^\"]".r) <~ "\"" ^^ { s => StringExp(s.mkString) }

  private def number: Parser[NumExp] = "[0-9]+".r ^^ { n => NumExp(n.toInt) }

  private def list: Parser[ListExp] = rep(expr) ^^ { ListExp }

  private def dottedList : Parser[DottedListExp] = rep(expr) ~ "." ~ expr ^^ { case head ~ "." ~ tail => new DottedListExp(head, tail) }

  private def quoted: Parser[ListExp] = "'" ~> expr ^^ { e => ListExp(List(SymbolExp("quote"), e)) }

  private def expr: Parser[Exp] = symbol | string | number | quoted | '(' ~> (dottedList | list) <~ space <~ ')'


  def apply(input: String): \/[String, Exp] =
    parse(expr, input) match {
      case Success(res, _) => res.right
      case NoSuccess(msg, _) => msg.left 
    }
  
}
  
