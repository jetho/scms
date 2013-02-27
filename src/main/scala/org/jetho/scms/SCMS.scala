
package org.jetho.scms

import scalaz._
import Scalaz._
import java.io._
import java.util.Scanner
import scala.annotation.tailrec


object SCMS {

  private val scanner = new Scanner(System.in)
 

  private def initEnv = 
    for {
      exp <- Reader.readExpr("(load \"stdlib.scm\")") 
      env =  EmptyEnvironment.extend(Primitives.primitives ::: Primitives.ioPrimitives)
      _   <- Eval.eval(env)(exp)
    } yield env

  @tailrec 
  private def until[A](pred: A => Boolean)(prompt: => A)(action: Environment => A => Unit)(env: Environment) {
    val res = prompt
    if (!pred(res)) {
      action(env)(res)
      until(pred)(prompt)(action)(env)
    }
  }
    
  private def readPrompt(s: String): String = {
    print(s); System.out.flush()
    scanner.nextLine
  }

  private def evalAndPrint(env: Environment)(expr: String) {
    val res = Reader.readExpr(expr) >>= Eval.eval(env)
    res.fold(errMsg => println("Error: " + errMsg), println)
  }

  private def run(action: Environment => Unit) =
    initEnv fold (printInitError, env => action(env))
  
  private def runRepl {
    println(welcomeMsg)
    run(env => until[String](_ == "quit")(readPrompt("SCMS>>> "))(evalAndPrint)(env))
    println(quitMsg)
  }

  private def runOne(file: String) {
    run(env => evalAndPrint(env)("(load \"" + file + "\")"))
  }


  def main(args: Array[String]) {
    args.toList match {
      case Nil => runRepl
      case file :: Nil => runOne(file)
      case _ => println("Program takes only 0 or 1 argument") 
    }
  }


  private def initError: ErrorMsg => String = s => s"Error while initializing the standard environment: $s"

  private def printInitError = initError andThen println

  private val welcomeMsg = "\n - Welcome to SCMS! -\n"
  private val quitMsg = "Bye!\n"
}
