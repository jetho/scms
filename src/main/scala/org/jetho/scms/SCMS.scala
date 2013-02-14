
package org.jetho.scms

import scalaz._
import Scalaz._
import java.io._
import java.util.Scanner
import scala.annotation.tailrec


object SCMS {

  val scanner = new Scanner(System.in)

  @tailrec 
  def until[A](pred: A => Boolean)(prompt: => A)(action: A => Unit) {
    val res = prompt
    if (!pred(res)) {
      action(res)
      until(pred)(prompt)(action)
    }
  }

  def runRepl {
    println(welcomeMsg)
    until[String](_ == "quit")(readPrompt("SCMS>>> "))(evalAndPrint)
    println(quitMsg)
  }

  def readPrompt(s: String): String = {
    print(s); System.out.flush()
    scanner.nextLine
  }

  def evalAndPrint(expr: String) {
    val res = Reader.read(expr) >>= Eval.eval
    res.fold(errMsg => println("Error: " + errMsg), println)
  }
  

  def main(args: Array[String]) {
    args.toList match {
      case Nil => runRepl
      case expression :: Nil => evalAndPrint(expression)
      case _ => println("Program takes only 0 or 1 argument") 
    }
  }


  val welcomeMsg = "\n - Welcome to SMCS! -\n"
  val quitMsg = "Bye!\n"
}
