package wacc.front_end

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.{File, PrintWriter}

class validProgramSemanticTest extends AnyFlatSpec {
  val directoryPath = "src/test/wacc/wacc-examples/valid/"
  val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)
  val finalPath = "src/test/wacc/wacc-examples/valid_trees/"


  for (file <- files) {
    val fileName = file.getPath
    val relativePath = fileName.stripPrefix(directoryPath)
    val newFilePath = finalPath + relativePath.stripSuffix(".wacc") + ".tree"

    val newFile = new File(newFilePath)
    val newDir = newFile.getParentFile
    if (!newDir.exists()) {
      newDir.mkdirs() // Create directories if they don't exist
    }

    if (fileName.endsWith(".wacc")) {
      val source = Source.fromFile(file)
      val fileContent = source.mkString
      source.close()

      it should s"successfully parse $fileName" in {
        parser.parse(fileContent) match {
          case Success(ast) => {
            val (prog, renamingErrors) = rename(ast)
            val (newProg, typeErrors) =  semantic.analyse(prog)
            assert(renamingErrors.isEmpty && typeErrors.isEmpty)

            val writer = new PrintWriter(newFile)
            writer.write(newProg.prettyPrint()) // Write your content
            writer.close()
          }
          case Failure(msg) => fail(s"$msg?")
        }
      }
    }
  }
}
