package wacc.back_end

import wacc.front_end._
import RegName._
import DataWidth._
import ExprGen._
import wacc.back_end.Stack.typeToSize
import scala.collection.mutable.{Builder}

// At the start of each assembly file
// .intel_syntax noprefix
// .globl main

object IR {
  val STACK_ALIGN = -16
  def generateIR(prog: Prog): (List[Section], List[LabelDef]) = {
    val funcLabelDefs = 
    funcGenerate(prog.main) :: prog.funcs.map(func => funcGenerate(func.stmts, func.identifier.identifier))
    val sections = List.newBuilder[Section] // each section is .rodata (read only)
                                          // what follows is .text
    return(sections.result(), funcLabelDefs)
  }

  // Data width defaults to QWord for moving pointers in registers
  def movQnToReg(destReg: RegName, qn: QualifiedName, dataWidth: DataWidth = QWord) =
   MOV(Reg(destReg, dataWidth), OffsetAddr(MemOpModifier.QWordPtr, Reg(Rbp, QWord), Stack.getOffset(qn)))

  def movRegOrImmToMem(srcRegOrImm: RegName | Imm, qn: QualifiedName, dataWidth: DataWidth = QWord) = {
    srcRegOrImm match
      case reg: RegName => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(reg, dataWidth))
      case imm: Imm => MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), imm)
  }
  def generateSection(prog: Prog) = {
    // This will generate the boilerplate at the beginning of the asm file and
    // the string+len of string stuff to go in that section

  }

  def funcGenerate(stmts: List[Stmt], funcName: String = ""): FuncLabelDef = {
    val asmBuilder = List.newBuilder[Instr]
    val localLabelBuilder = List.newBuilder[LocalLabelDef]
    Stack.initialise(stmts)

    asmBuilder += PUSH(Reg(Rbp, QWord)) //
    asmBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord)) //
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth)) //
    stmts.map(stmtGen(_, asmBuilder, localLabelBuilder))
    
    val funcLabel = if (funcName == "") {
      asmBuilder += MOV(Reg(Rax, QWord), Imm(0))
      "main"
    } else s"wacc_$funcName"

    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth)) 
    asmBuilder += POP(Reg(Rbp, QWord)) 
    asmBuilder += RET 
    
    return FuncLabelDef(funcLabel, asmBuilder, localLabelBuilder)

  }

  def stmtGen(stmt: Stmt, asmBuilder: Builder[Instr, List[Instr]], localLabelBuilder: Builder[LocalLabelDef, List[LocalLabelDef]]): Unit =
    (stmt: @unchecked) match {
      case Skip => ()
      case Assgn(t, qn: QualifiedName, rValue) => assgnGen(t, qn, rValue, asmBuilder)
      case ReAssgn(lValue, rValue) => reassgnGen(lValue, rValue, asmBuilder)
      case Exit(expr) => exitGen(expr, asmBuilder)
      case Return(expr) => returnGen(expr, asmBuilder)
      case Read(lValue) => readGen(lValue, asmBuilder)
      case Free(expr) => freeGen(expr, asmBuilder)
      case Scope(stmts) => scopeGen(stmts, asmBuilder, localLabelBuilder)
      case WhileDo(condition, stmts) => ???
      case IfElse(condition, thenStmts, elseStmts) => ???
      case Print(expr) => ???
      case Println(expr) => ???

    }

  def assgnGen(t: Type, qn: QualifiedName, rValue: RValue, asmBuilder: Builder[Instr, List[Instr]]) : Builder[Instr, List[Instr]] = {
    val offset = Stack.getOffset(qn)
    val dataWidth = Stack.typeToSize(t)
    storeOnStack(rValue, dataWidth, offset, asmBuilder, t)
  }

  def reassgnGen(lValue: LValue, rValue: RValue, asmBuilder: Builder[Instr, List[Instr]]) = {
    (lValue: @unchecked) match
        case qn: QualifiedName => assgnGen(qn.t, qn, rValue, asmBuilder)
        case ArrayElem(qn: QualifiedName, index) => {
          val (dataWidth, pointer) = repeatAccessArray(qn, index, asmBuilder)
          asmBuilder += MOV(pointer, rValueGen(rValue, asmBuilder, qn.t))
        }
        case Fst(lValue2) => {
          fstSndAddress(lValue2, asmBuilder) // Store address in R10
          asmBuilder += MOV(Reg(9, QWord), Reg(10, QWord)) // Move into R9
          val regOrImm: (Reg | Imm) =
          rValueGen(rValue, asmBuilder) match
            case Reg(num, dataWidth) => Reg(num, QWord)
            case Imm(value) => Imm(value)
          asmBuilder += MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(R9, QWord)), regOrImm) // Store RValue in R10, MOV R10 into [R9]
        }
        case Snd(lValue2) => {
          fstSndAddress(lValue2, asmBuilder, 8)
          asmBuilder += MOV(Reg(9, QWord), Reg(10, QWord))
          val regOrImm: (Reg | Imm) =
          rValueGen(rValue, asmBuilder) match
            case Reg(num, dataWidth) => Reg(num, QWord)
            case Imm(value) => Imm(value)
          asmBuilder += MOV(OffsetAddr(MemOpModifier.QWordPtr, Reg(R9, QWord)), regOrImm)
        }
  }

  def arrayIndexAccess(pointerAddress:MemAddr, index: Expr, t: Type, asmBuilder: Builder[Instr, List[Instr]]): (Int, MemAddr) = {
    asmBuilder += MOV(Reg(R9, QWord), pointerAddress)
    val dataWidth = typeToSize(t)
    asmBuilder.addAll(
      List(
        MOV(Reg(R8, DWord), exprGen(index, asmBuilder)), // Okay because index has to be an int
        CMP(Reg(R8, DWord), Imm(0)),
        JCond(Cond.L, Label("_outOfBounds")),
        CMP(Reg(R8, DWord), OffsetAddr(MemOpModifier.DWordPtr, Reg(R9, QWord), -4)),
        JCond(Cond.GE, Label("_outOfBounds"))
      )
    )
    (dataWidth, RegScale(dataWidth, Reg(R9, QWord), typeToSize(t), Reg(R8, DWord)))
  }

  def rValueGen(rValue: RValue, asmBuilder: Builder[Instr, List[Instr]], t: Type = Undefined): (Reg | Imm) = {
    (rValue: @unchecked) match
      case expr: Expr => exprGen(expr, asmBuilder)
      case Fst(lValue) => fstSndAddress(lValue, asmBuilder)
      case Snd(lValue) => fstSndAddress(lValue, asmBuilder, 8)
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], asmBuilder)
      case NewPair(fst, snd) => mallocNewPair(fst, snd, asmBuilder)
      case Call(qn: QualifiedFunc, args) => callGen(qn, args, asmBuilder)
  }

  def storeOnStack(rValue: RValue, dataWidth: Int, offset: Int, asmBuilder: Builder[Instr, List[Instr]], t: Type = Undefined) = {
    rValue match
      case expr: Expr => asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), offset), exprGen(expr, asmBuilder))
      case Fst(lValue) => fstSndAddress(lValue, asmBuilder); asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), offset), Reg(R10, dataWidth))
      case Snd(lValue) => fstSndAddress(lValue, asmBuilder, 8); asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), offset), Reg(R10, dataWidth))
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], asmBuilder); asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), offset), Reg(Rax, dataWidth))
      case NewPair(fst, snd) => mallocNewPair(fst, snd, asmBuilder); asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), offset), Reg(Rax, dataWidth))
      case Call(qn: QualifiedFunc, args) => callGen(qn, args, asmBuilder); asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), offset), Reg(Rax, dataWidth))
  }

  def mallocArrayLiter(elems: List[Expr], arrayType: ArrayType, asmBuilder: Builder[Instr, List[Instr]]) = {

    val arrayTypeSize = if arrayType.d != 1 then typeToSize(arrayType) else typeToSize(arrayType)
    asmBuilder.addAll(
      List(
        MOV(Reg(Rdi, DWord), Imm(arrayTypeSize*(elems.size) + 4)),
        CALL(Label("_malloc")),
        MOV(OffsetAddr(MemOpModifier.DWordPtr, Reg(Rax, QWord)), Imm(elems.size)),
        ADD(Reg(Rax, DWord), Imm(4))
        )
    )
    elems.indices.map(i => {
      asmBuilder += MOV(OffsetAddr(arrayTypeSize, Reg(Rax, QWord), i*arrayTypeSize), exprGen(elems(i), asmBuilder)) // will be fine
    })
    Reg(Rax, QWord)
  }

  def mallocNewPair(fst: Expr, snd: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val (fstSize, sndSize) = (8, 8)
    asmBuilder.addAll(
    List(
      MOV(Reg(Rdi, DWord), Imm(fstSize+sndSize)),
      CALL(Label("_malloc")),
      )
    )
    asmBuilder += MOV(OffsetAddr(fstSize, Reg(Rax, QWord)), exprGen(fst, asmBuilder)) // will be fine
    asmBuilder += MOV(OffsetAddr(sndSize, Reg(Rax, QWord), sndSize), exprGen(snd, asmBuilder)) // will be fine
    Reg(Rax, QWord)
  }


  def repeatAccessArray(qn: QualifiedName, index: List[Expr], asmBuilder: Builder[Instr, List[Instr]]): (Int, MemAddr) = {
    val arrayElemType = qn.t.asInstanceOf[ArrayType]
    var pointer: MemAddr = OffsetAddr(MemOpModifier.QWordPtr, Reg(Rbp, QWord), Stack.getOffset(qn))
    for (i <- 1 to index.size - 1) pointer = arrayIndexAccess(pointer, index(i), arrayElemType, asmBuilder)._2
    arrayIndexAccess(pointer, index.last, arrayElemType.t, asmBuilder)
  }

  def readFunc(dataWidth: Int,asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder.addAll(
      List(
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        SUB(Reg(Rsp, QWord), Imm(-STACK_ALIGN)),
        MOV(OffsetAddr(dataWidth, Reg(Rsp, QWord)), Reg(Rdi, dataWidth)),
        LEA(Reg(Rsi, QWord), OffsetAddr(MemOpModifier.QWordPtr, Reg(Rsp, QWord))),
      //LEA(Reg(Rsi, QWord), [rip + .L._readi_str0]), lea rdi, [rip + .L._readi_str0],
        MOV(Reg(Rax, Byte), Imm(0)),
        CALL(Label("scanf@plt")),
        MOV(Reg(Rax, dataWidth), OffsetAddr(dataWidth, Reg(Rsp, QWord))),
        ADD(Reg(Rsp, QWord), Imm(-STACK_ALIGN)),
      )
    )
  }

  def readGen(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]]) = {
      (lValue: @unchecked) match
        case qn: QualifiedName => {
          val dataWidth = typeToSize(qn.t)
          asmBuilder += movQnToReg(Rdi, qn, dataWidth) // Check if bugs later
          // asmBuilder += MOV(Reg(Rdi, dataWidth), OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)))
          readFunc(dataWidth, asmBuilder)
          asmBuilder += movRegOrImmToMem(Rax, qn, dataWidth) // Check if bugs later
          // asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(Rax, dataWidth))
        }
        case ArrayElem(qn: QualifiedName, index) => {
          val arrayBaseType = qn.t.asInstanceOf[ArrayType].t
          val arrayPointer = OffsetAddr(MemOpModifier.QWordPtr, Reg(Rbp, QWord), Stack.getOffset(qn))
          val (dataWidth, pointer) =  arrayIndexAccess(arrayPointer, index.last, arrayBaseType, asmBuilder)
          asmBuilder += MOV(Reg(Rdi, dataWidth), pointer)
          readFunc(dataWidth, asmBuilder)
          asmBuilder += MOV(pointer, Reg(Rax, dataWidth))
        }
        case Fst(lValue) => readPair(lValue, asmBuilder, 0)

        case Snd(lValue) => readPair(lValue, asmBuilder , 8)
    }

  def fstSndAddress(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]], fstOrSnd: Int = 0): Reg = {
    val storeInstrs = List(
      CMP(Reg(10, QWord), Imm(0)),
      JCond(Cond.E, Label("_errNull")),
      MOV(Reg(10, QWord), OffsetAddr(MemOpModifier.QWordPtr, Reg(R10, QWord), fstOrSnd))
    )
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        movQnToReg(R10, qn)
        asmBuilder.addAll(storeInstrs)
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val (dataWidth, pointer) = repeatAccessArray(qn, index, asmBuilder)
        asmBuilder += MOV(Reg(10, QWord), pointer)
        asmBuilder.addAll(storeInstrs)
      }
      case Fst(lValue) => {
        fstSndAddress(lValue, asmBuilder)
        asmBuilder.addAll(storeInstrs)
      }
      case Snd(lValue) => {
        fstSndAddress(lValue, asmBuilder, 8)
        asmBuilder.addAll(storeInstrs)
      }
      Reg(R10, QWord)
    }

  def readPair(lValue: LValue, asmBuilder: Builder[Instr, List[Instr]], fstOrSnd: Int) = {
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        val dataWidth = typeToSize(qn.t)
        fstSndAddress(lValue, asmBuilder, fstOrSnd)
        asmBuilder += MOV(Reg(Rdi, dataWidth), OffsetAddr(dataWidth, Reg(R10, QWord)))
        readFunc(dataWidth, asmBuilder)
        asmBuilder += MOV(OffsetAddr(dataWidth, Reg(R10, QWord)), Reg(Rax, dataWidth))
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val dataWidth = typeToSize(qn.t.asInstanceOf[ArrayType].t)
        fstSndAddress(lValue, asmBuilder, fstOrSnd)
        asmBuilder += MOV(Reg(Rdi, dataWidth), OffsetAddr(dataWidth, Reg(R10, QWord)))
        readFunc(dataWidth, asmBuilder)
        asmBuilder += MOV(OffsetAddr(dataWidth, Reg(R10, QWord)), Reg(Rax, dataWidth))
      }
  }

  def exitGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder.addAll(
      List(
        MOV(Reg(Rdi, DWord), exprGen(expr, asmBuilder)), // expr can be assumed to be an int
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),       // exit can only take an int
        CALL(Label("exit@plt"))
      )
    )
  }

  def freeGen(expr: Expr, asmBuilder:Builder[Instr, List[Instr]]) = {
    (expr: @unchecked) match
      case qn: QualifiedName if qn.t.isInstanceOf[ArrayType] => {
        asmBuilder.addAll(
          List(
            movQnToReg(Rdi, qn),
            SUB(Reg(Rdi, QWord), Imm(4)),
            AND(Reg(Rsp, QWord), Imm(-16)),
            CALL(Label("free@plt")),
            MOV(Reg(Rax, QWord), Imm(0))
          )
        )
      }
      case qn: QualifiedName if qn.t.isInstanceOf[PairElemType] => {
        asmBuilder += movQnToReg(Rdi, qn)
        asmBuilder.addAll(
          List(
            AND(Reg(Rsp, QWord), Imm(-16)),
            CMP(Reg(Rdi, QWord), Imm(0)),
            JCond(Cond.E, Label("_errNull")),
            CALL(Label("free@plt")),
            MOV(Reg(Rax, QWord), Imm(0))
          )
        )
      }
    }

  def scopeGen(stmts: List[Stmt], asmBuilder: Builder[Instr, List[Instr]], localLabelBuilder: Builder[LocalLabelDef, List[LocalLabelDef]]) = {
    val prevFrame = Stack.frames.last
    Stack.frames += StackFrame(stmts, prevFrame.identTable, prevFrame.absoluteDepth())
    print(Stack.frames.last.identTable)
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    stmts.map(stmtGen(_, asmBuilder, localLabelBuilder))
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    Stack.frames.dropRightInPlace(1)
  }

  def returnGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]]) = {
    val reg = exprGenRegister(expr, asmBuilder)
    asmBuilder += MOV(Reg(Rax, reg.dataWidth), reg)
  }

  def callGen(qn: QualifiedFunc, args: List[Expr], asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder += PUSH(Reg(Rbp, QWord))
    asmBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    val spaceNeeded = qn.paramTypes.foldLeft(0)(_ + typeToSize(_))
    asmBuilder += SUB(Reg(Rsp, QWord), Imm(spaceNeeded))
    var currentDepth = 0
    for ((paramType, i) <- qn.paramTypes.zipWithIndex) {
      val dataWidth = typeToSize(paramType)
      currentDepth -= dataWidth
      asmBuilder += MOV(OffsetAddr(dataWidth, Reg(Rbp, QWord), currentDepth), exprGen(args(i), asmBuilder))
    }
    asmBuilder += CALL(Label(s"wacc_${qn.funcName}"))
    asmBuilder += ADD(Reg(Rsp, QWord), Imm(spaceNeeded))
    asmBuilder += POP(Reg(Rbp, QWord))
    Reg(Rax, typeToSize(qn.t))
  }

}
