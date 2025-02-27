package wacc.front_end
import wacc.back_end

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.io.File
import java.nio.file.{Files, Paths}
import wacc.back_end.IR
import wacc.back_end.AssemblyWriter
import wacc.back_end.IR.generate
import wacc.back_end.AssemblyWriter.generateAsmFile
import scala.sys.process._

class produceAssembly extends AnyFlatSpec {

  val binDir: String = "src/test/wacc/back_end_tests/bin"

  def findOutputValues(filePath: String): List[String] = {
    val source = Source.fromFile(filePath)
    try {
      val lines = source.getLines()
        .filter(_.startsWith("#")) // Find all lines starting with "#"
        .map(_.stripPrefix("#").trim) // Remove "#" and trim whitespace
        .map(line => "  " + line)
        .toList
      
      // Find indices of "Output:" and "Program:" markers
      val outputIndex = lines.indexWhere(_.contains("Output:"))
      val programIndex = lines.indexWhere(_.contains("Program:"))
      
      // Return only lines between "Output:" and "Program:"
      if (outputIndex >= 0 && programIndex > outputIndex) {
        lines.slice(outputIndex + 1, programIndex)
          .filter(_.nonEmpty) // Remove empty lines
          .map(_.replaceAll("\\s", "")) // Remove all whitespace characters
      } else {
        List() // Return empty list if markers not found or in wrong order
      }
    } finally {
      source.close()
    }
  }

  val directoryPath = "src/test/wacc/wacc-examples/valid/basic"
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
            
            val baseName = new File(fileName).getName.replace(".wacc", "")
            val assemblyFilepath = s"assemblyFiles/${baseName}.s"
            
            

            val binDirPath = new File(binDir)
            if (!binDirPath.exists()) {
              binDirPath.mkdirs()
            }

            val compilation = Process(s"gcc -z noexecstack -o ${binDir}/${baseName} ${assemblyFilepath}").!
            println(s"\nCompilation exit code: ${compilation}")
            
            
            // println(s"Output binary exists?: ${File(s"./${binDir}/${baseName}").exists()}")
            // println(s"assembly file path is ${assemblyFilepath}")
            
            val exitCode = Process(s"./${binDir}/$baseName").!
            // val output = Process(s"./${binDir}/$baseName").!!
            
            println(s"Program exitCode: $exitCode")
            // println(s"Program output: $output")
            
            // println(s"Original Wacc file ?: ${File(s"${file.getPath}").exists()}")
            val expectedResult = findOutputValues(file.getPath) 

            println(s"Comments are: $expectedResult")

            if (expectedResult.isEmpty) {
              println("Not expecting any exit codes or returns")
              assert(exitCode.toString == "0")
            } else if (expectedResult(0) == "Exit:") {
              val expected = expectedResult(1) 
              println(s"Expecting exit code of $expected")
              assert(expected == exitCode.toString)
            }

            // if (expectedResult.contains(output.toString())) {
            //   println("Output matches result")
            // } else {
            //   println(s"expected ${value} but got ${output}")
            // }
          }
          case Failure(msg) => fail(s"$msg?")
        }
      }
    }
  }
}