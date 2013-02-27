
package org.jetho.scms

import scalaz._
import Scalaz._
import java.io._
import java.util.Scanner
import scala.annotation.tailrec


object SCMS {

  val scanner = new Scanner(System.in)
 
  
  def initialEnv = EmptyEnvironment.extend(Primitives.primitives ::: Primitives.ioPrimitives)

  def loadStdLib(env: Environment) = 
    Reader.readExpr("(load \"stdlib.scm\")") >>= Eval.eval(env)    

  def initEnv = 
    for {
      env <- initialEnv.right
      _   <- loadStdLib(env)
    } yield env

  @tailrec 
  def until[A](pred: A => Boolean)(prompt: => A)(action: Environment => A => Unit)(env: Environment) {
    val res = prompt
    if (!pred(res)) {
      action(env)(res)
      until(pred)(prompt)(action)(env)
    }
  }
    
  def readPrompt(s: String): String = {
    print(s); System.out.flush()
    scanner.nextLine
  }

  def evalAndPrint(env: Environment)(expr: String) {
    val res = Reader.readExpr(expr) >>= Eval.eval(env)
    res.fold(errMsg => println("Error: " + errMsg), println)
  }

  def run(action: Environment => Unit) =
    initEnv fold (printInitError, env => action(env))

  
  def runRepl {
    println(welcomeMsg)
    run(env => until[String](_ == "quit")(readPrompt("SCMS>>> "))(evalAndPrint)(env))
    println(quitMsg)
  }

  def runOne(file: String) {
    run(env => evalAndPrint(env)("(load \"" + file + "\")"))
  }


  def main(args: Array[String]) {
    args.toList match {
      case Nil => runRepl
      case file :: Nil => runOne(file)
      case _ => println("Program takes only 0 or 1 argument") 
    }
  }


  def initError: ErrorMsg => String = s => s"Error while initializing the standard environment: $s"

  def printInitError = initError andThen println

  val welcomeMsg = "\n - Welcome to SCMS! -\n"
  val quitMsg = "Bye!\n"
}
