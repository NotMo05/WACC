package wacc.front_end

import scala.io.Source
import parsley.{Success, Failure}
import java.io.File
import wacc.back_end.ExprGen
import wacc.back_end.ExprGen._
import wacc.back_end.Stack
import wacc.back_end.IR.mainGenerate
import wacc.back_end.IR.generate
val SUCCESS = 0
val SYNTAX_ERR = 100
val SEMANTIC_ERR = 200

def main(args: Array[String]): Unit = {
  val fileName = args(0)
  val file = new File(fileName)
  val fileContent = Array(Source.fromFile(file).mkString)
  fileContent.headOption match {
    // Parse and conduct syntax analysis on the program string
    case Some(progString) => parser.parse(progString) match {
      case Success(ast) => {
        val (newProg, errors) = rename(ast)
        if !(errors.isEmpty && semantic.analyse(newProg).isEmpty) then {
          println("Semantic Errors:")
          semantic.analyse(newProg).foreach(println(_))
          sys.exit(SEMANTIC_ERR)
        } else {
          generate(newProg)
          print("No Error")
          sys.exit(SUCCESS)
        }
      }
      case Failure(msg) => {
        print("Syntax Error")
        print(msg)
        sys.exit(SYNTAX_ERR)}
    }
      case None => println("Please enter an expression")
    }
  }
