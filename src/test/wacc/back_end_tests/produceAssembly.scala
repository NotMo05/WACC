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
import scala.sys.process._

class produceAssembly extends AnyFlatSpec {

  def findOutputValue(filePath: String): Option[String] = {
    val source = Source.fromFile(filePath)
    try {
      source.getLines()
        .find(_.startsWith("output:")) // Find the first line starting with "output:"
        .map(_.stripPrefix("output:").trim) // Remove "output:" and trim whitespace
    } finally {
      source.close()
    }
  }

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

            //TODO GET ASSEMBLY FILE PATH
            val baseName = new File(fileName).getName.replace(".wacc", "")
            val assemblyFilepath = s"assemblyFiles/${baseName}.s"

            val compileProcess = Process(s"gcc -o output_binary ${assemblyFilepath}")
            val compileExitCode = compileProcess.!
            
            var output = new StringBuilder
            var errorOutput = new StringBuilder

            val logger = ProcessLogger(
              (out: String) => output.append(out + "\n"),  // Capture stdout
              (err: String) => errorOutput.append(err + "\n") // Capture stderr
            )

            val exitCode = Process("./output_binary").!(logger)

            println(s"Program output:\n$output")
            println(s"Error output:\n$errorOutput")
            
            val expectedResult = findOutputValue(assemblyFilepath) 

            if (expectedResult.contains(output.toString())) {
              println("Output matches result")
            } else {
              println(s"expected ${value} but got ${output}")
            }
          }
          case Failure(msg) => fail(s"$msg?")
        }
      }
    }
  }
}