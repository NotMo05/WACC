package wacc
import parsley.{Success, Failure}
import scala.io.Source



def main(args: Array[String]): Unit = {
    println("hello WACC!")

    val filePath = "src/test/wacc/wacctest.wacc"
    val fileContent = Source.fromFile(filePath).mkString
    Source.fromFile(filePath).close()

    val arguments = if args.isEmpty then Array(fileContent) else args

    arguments.headOption match {
        case Some(expr) => parser.parse(expr) match {
            case Success(x) => println(s"$expr = $x")
            case Failure(msg) => println(msg)
        }
        case None => println("please enter an expression")
    }
}
