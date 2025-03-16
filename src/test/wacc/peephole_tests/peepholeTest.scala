package wacc.front_end

import wacc.back_end
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import scala.io.Source
import java.nio.file.{Files, Paths}
import java.io.File
import wacc.back_end.IR
import wacc.back_end.AssemblyWriter
import wacc.back_end.AssemblyWriter.generateAsmFile
import scala.sys.process._
import org.scalatest.BeforeAndAfter
import wacc.back_end.IR.StringInfo
import wacc.back_end._
import wacc.peephole.Peephole

class peepholeTest extends AnyFlatSpec with BeforeAndAfter {
  val RspAdjMovOverwriteIR = {
    (
      List.empty[StringInfo], 
      List(
        FuncLabelDef(
          "main", 
          List.newBuilder.addAll(
            List(
              ADD(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8),
              ),
              MOV(
                Reg(RegName.Rsp, DataWidth.QWord),
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              ADD(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8),
              ),
              MOV(
                Reg(RegName.Rsp, DataWidth.QWord),
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              ADD(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8),
              ),
              MOV(
                Reg(RegName.Rsp, DataWidth.QWord),
                Reg(RegName.Rbp, DataWidth.QWord)
              )
            )
          )
        )
      )
    ) 
  }

  val optimisedRspAdjMovOverwriteIR = {
    (
      List.empty[StringInfo], 
      List(
        FuncLabelDef(
          "main", 
          List.newBuilder.addAll(
            List(
              MOV(
                Reg(RegName.Rsp, DataWidth.QWord),
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              MOV(
                Reg(RegName.Rsp, DataWidth.QWord),
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              MOV(
                Reg(RegName.Rsp, DataWidth.QWord),
                Reg(RegName.Rbp, DataWidth.QWord)
              )
            )
          )
        )
      )
    )
  }

  it should "successfully remove assembly for RspAdjMovOverwriteIR" in {
    val optimisedIRFuncLabelDefs = RspAdjMovOverwriteIR._2.map(Peephole.optimiseFuncLabelDef)
    val IR = (RspAdjMovOverwriteIR._1, optimisedIRFuncLabelDefs)
    assert(IR == optimisedRspAdjMovOverwriteIR)
  }
}

