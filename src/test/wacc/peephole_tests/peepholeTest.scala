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

  val PushPopIR = {
    (
      List.empty[StringInfo], 
      List(
        FuncLabelDef(
          "main", 
          List.newBuilder.addAll(
            List(
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              )
            )
          )
        )
      )
    )
  }

  val optimisedPushPopIR = {
    (
      List.empty[StringInfo], 
      List(
        FuncLabelDef(
          "main", 
          List.newBuilder.addAll(
            List(
              POP(
                Reg(RegName.Rbp, DataWidth.QWord)
              ),
              PUSH(
                Reg(RegName.Rbp, DataWidth.QWord)
              )
            )
          )
        )
      )
    )
  }

  val RspAddSubCancelIR = {
    (
      List.empty[StringInfo], 
      List(
        FuncLabelDef(
          "main", 
          List.newBuilder.addAll(
            List(
              ADD(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8)
              ),
              SUB(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8)
              ),
              ADD(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8)
              ),
              SUB(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8)
              ),
              ADD(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8)
              ),
              SUB(
                Reg(RegName.Rsp, DataWidth.QWord),
                Imm(8)
              )
            )
          )
        )
      )
    )
  }

  val optimisedRspAddSubCancelIR = {
    (
      List.empty[StringInfo], 
      List(
        FuncLabelDef(
          "main", 
          List.newBuilder.addAll(
            List(
            )
          )
        )
      )
    )
  }

  it should "successfully remove ADDs from RspAdjMovOverwriteIR" in {
    val optimisedIRFuncLabelDefs = RspAdjMovOverwriteIR._2.map(Peephole.optimiseFuncLabelDef)
    val IR = (RspAdjMovOverwriteIR._1, optimisedIRFuncLabelDefs)
    assert(IR == optimisedRspAdjMovOverwriteIR)
  }

  it should "successfully remove all instructions from PushPopOrPopPushIR" in {
    val optimisedIRFuncLabelDefs = PushPopIR._2.map(Peephole.optimiseFuncLabelDef)
    val IR = (PushPopIR._1, optimisedIRFuncLabelDefs)
    assert(IR == optimisedPushPopIR)
  }

  it should "successfully remove all instructions from RspAddSubCancelIR" in {
    val optimisedIRFuncLabelDefs = RspAddSubCancelIR._2.map(Peephole.optimiseFuncLabelDef)
    val IR = (RspAddSubCancelIR._1, optimisedIRFuncLabelDefs)
    assert(IR == optimisedRspAddSubCancelIR)
  }
}

