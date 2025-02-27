package wacc.front_end
import wacc.back_end

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.{File}
import wacc.back_end.IR
import wacc.back_end.AssemblyWriter
import wacc.back_end.IR.generate
import wacc.back_end.AssemblyWriter.generateAsmFile

class produceAssembly extends AnyFlatSpec {
  val directoryPath = "src/test/wacc/wacc-examples/valid/"
  val files = FileUtils.listAllFiles(new File(directoryPath)).filter(_.isFile)


  for (file <- files) {
    val fileName = file.getPath

    if (fileName.endsWith(".wacc")) {
      val source = Source.fromFile(file)
      val fileContent = source.mkString
      source.close()

      it should s"successfully produce assembly for $fileName" in {
        // pending
        parser.parse(fileContent) match {
          case Success(ast) => {
            val (newProg, errors) = rename(ast)
            assert(errors.isEmpty && semantic.analyse(newProg).isEmpty)
            val mainLabel = generate(newProg)
            generateAsmFile((List(), List(mainLabel)), fileName, "assemblyFiles/")
          }
          case Failure(msg) => fail(s"$msg?")
        }
      }
    }
  }
}
