package wacc.peephole
import wacc.back_end._
import wacc.back_end.IR._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Peephole {

  def optimiseFuncLabelDef(funcLabelDef: FuncLabelDef): FuncLabelDef = {
    val funcLabelDefInstrArrayBuff = mutable.ArrayBuffer.from(funcLabelDef.instructions.result())
    var instrNum = 0

    def isPushPopOrPopPush(instrNum: Int): Boolean = {
      if (instrNum == funcLabelDefInstrArrayBuff.size - 1) then false
      else {
      funcLabelDefInstrArrayBuff(instrNum) match
        case PUSH(op1) => funcLabelDefInstrArrayBuff(instrNum + 1) match
          case POP(op2) if op1 == op2 => true
          case _ => false
        case POP(op1) => funcLabelDefInstrArrayBuff(instrNum + 1) match
          case PUSH(op2) if op1 == op2 => true
          case _ => false
        case _ => false
      }
    }
    
    def isRspAdjMovOverwrite(instrNum: Int): Boolean = {
      if (instrNum == funcLabelDefInstrArrayBuff.size - 1) then false
      else {
        funcLabelDefInstrArrayBuff(instrNum) match
          case ADD((Reg(RegName.Rsp, dataWidth1), _)) => funcLabelDefInstrArrayBuff(instrNum + 1) match
            case MOV((Reg(RegName.Rsp, dataWidth2), _)) if dataWidth1 == dataWidth2 => true
            case MOVZX((Reg(RegName.Rsp, dataWidth2), _)) if dataWidth1 == dataWidth2 => true
            case _ => false
          case _ => false
      }
    }

    while (instrNum < funcLabelDefInstrArrayBuff.size) {
      var anOptimisationHasBeenDone = false
      if (isPushPopOrPopPush(instrNum)) {
        funcLabelDefInstrArrayBuff.remove(instrNum, instrNum + 1)
        anOptimisationHasBeenDone = true
      }
      if (isRspAdjMovOverwrite(instrNum)) {
        funcLabelDefInstrArrayBuff.remove(instrNum)
        anOptimisationHasBeenDone = true
      }
      if (!anOptimisationHasBeenDone) then instrNum += 1
    }
    FuncLabelDef(funcLabelDef.name, List.newBuilder.addAll(funcLabelDefInstrArrayBuff.toList))
  }
}