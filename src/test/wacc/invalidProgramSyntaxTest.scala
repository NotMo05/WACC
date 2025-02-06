package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.File

class invalidProgramSyntaxTest extends AnyFlatSpec {
  val directoryPath = "wacc-examples/invalid/syntaxErr"
  val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)

  for (file <- files) {
    val fileName = file.getPath
    val source = Source.fromFile(file, "UTF-8")
    val fileContent = source.mkString
    source.close()

    it should s"fail to parse $fileName" in {
      parser.parse(fileContent) match {
        case Failure(_) =>
        case Success(_) => fail(s"This should fail")
      }
    }
  }
}