package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.File

class validProgramRenameTest extends AnyFlatSpec {
  val directoryPath = "wacc-examples/valid/"
  val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)

  for (file <- files) {
    val fileName = file.getPath
    val source = Source.fromFile(file)
    val fileContent = source.mkString
    source.close()

    it should s"successfully parse $fileName" in {
      parser.parse(fileContent) match {
        case Success(ast) => {
          val (_, errors) = rename(ast)
          assert(errors.isEmpty)
        }
        case Failure(msg) => fail(s"$msg?")
      }
    }
  }
}