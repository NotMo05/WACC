package wacc

import scala.io.Source
import parsley.{Success, Failure}
import java.io.File

  


def main(args: Array[String]): Unit = {
  println("hello WACC!")
  // function/simple_functions/ fails
  // scope/ fails begin stmt end doesnt allow begin stmts end
  // runtimeErr/integerOverflow/ fails  Negation tick, Subtraction tick, negative numbers X

  val directoryPath = "src/test/wacc/wacc-examples/valid/function/simple_functions/"
  val files = new File(directoryPath).listFiles()
  val fileNames = files.filter(_.isFile).map(_.getName).toList
  print(fileNames.toString())

  for (fileName <- fileNames) {
    val filePath = directoryPath + fileName
    val fileContent = Source.fromFile(filePath).mkString
    Source.fromFile(filePath).close()


    val arguments = if args.isEmpty then Array(fileContent) else args

    arguments.headOption match {
      case Some(expr) => parser.parse(expr) match {
        case Success(x) => println(s"$expr = $x")
        case Failure(msg) => {
          println(msg)
          throw Error("HI")
        }
      }
        case None => println("please enter an expression")
      }
    }
  }

