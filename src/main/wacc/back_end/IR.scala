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
    ???
  }
  val sections = List.newBuilder[Section] // each section is .rodata (read only)
                                          // what follows is .text
  val labelDefs = List.newBuilder[LabelDef] // each function may contain labels/jumping points (ie for loops)

  // Data width defaults to QWord for moving pointers in registers
  def movQnToReg(destReg: RegName, qn: QualifiedName, dataWidth: DataWidth = QWord) =    
   MOV(Reg(destReg, dataWidth), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp, QWord), Stack.getOffset(qn)))

  def movRegOrImmToMem(srcRegOrImm: RegName | Imm, qn: QualifiedName, dataWidth: DataWidth = QWord) = {
    srcRegOrImm match
      case reg: RegName => MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(reg, dataWidth))
      case imm: Imm => MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), Stack.getOffset(qn)), imm)
  }
  def generateSection(prog: Prog) = {
    // This will generate the boilerplate at the beginning of the asm file and
    // the string+len of string stuff to go in that section

  }

  def generate(prog: Prog) = prog match {
    case Prog(_, main) =>
      mainGenerate(main)
  }

  def mainGenerate(stmts: List[Stmt]): LabelDef = {
    val instructionBuilder = List.newBuilder[Instr]
    instructionBuilder += PUSH(Reg(Rbp, QWord))
    instructionBuilder += MOV(Reg(Rbp, QWord), Reg(Rsp, QWord))
    Stack.initialise(stmts)
    instructionBuilder += SUB(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    println(Stack.frames.last.identTable)
    stmts.map(stmtGen(_, instructionBuilder))

    instructionBuilder += MOV(Reg(Rax, QWord), Imm(0))
    instructionBuilder += ADD(Reg(Rsp, QWord), Imm(-Stack.frames.last.currentDepth))
    instructionBuilder += POP(Reg(Rbp, QWord))
    instructionBuilder += RET
    return LocalLabelDef("main", instructionBuilder)
  }

  def stmtGen(stmt: Stmt, builder: Builder[Instr, List[Instr]]) = stmt match {
    case Skip => Nil
    case Assgn(t, qn: QualifiedName, rValue) => assgnGen(t, qn, rValue, builder)
    case ReAssgn(lValue, rValue) => reassgnGen(lValue, rValue, builder)
    case Exit(expr) => exitGen(expr, builder)
    case Read(lValue) => readGen(lValue, builder)

    case Free(expr) => {
      (expr: @unchecked) match 
        case qn: QualifiedName if qn.t.isInstanceOf[ArrayType] => {
        builder += movQnToReg(Rdi, qn)
        }
        case qn: QualifiedName if qn.t.isInstanceOf[PairElemType] => {
          ??? // qn.
        }



      // val offset = Stack.getOffset()

        // val pairOffset = Stack.getOffset(qn)
        // builder += MOV(Reg(10, QWord), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp, QWord), pairOffset))
        // builder.addAll(storeInstrs)



    //   builder.addAll(
    //     List(
    //       MOV()
    //     )
    //   )
    // }
    

	// mov dword ptr [r11 + 8], 3
	// mov r12, r11
	// # array pointers are shifted forward by 4 bytes, so correct it back to original pointer before free
	// mov rdi, r12
	// sub rdi, 4
	// # statement primitives do not return results (but will clobber r0/rax)
	// call _free
	// mov rax, 0
	// # pop/peek {rbx, r12}
	// mov rbx, qword ptr [rsp]


// _free:
// 	push rbp
// 	mov rbp, rsp
// 	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
// 	and rsp, -16
// 	call free@plt
// 	mov rsp, rbp
// 	pop rbp
// 	ret



      }

    case Return(expr) => ???
    case Scope(stmts) => ???
    case WhileDo(condition, stmts) => ???
    case IfElse(condition, thenStmts, elseStmts) => ???
    case Print(expr) => ???
    case Println(expr) => ???
    case wacc.front_end.Assgn(_, wacc.front_end.Ident(_), _) => ???  }

  def assgnGen(t: Type, qn: QualifiedName, rValue: RValue, builder: Builder[Instr, List[Instr]]) : Builder[Instr, List[Instr]] = {
    val offset = Stack.getOffset(qn)
    val dataWidth = Stack.typeToSize(t)
    storeOnStack(rValue, dataWidth, offset, builder, t)
  }
  
  def reassgnGen(lValue: LValue, rValue: RValue, builder: Builder[Instr, List[Instr]]) = {
    (lValue: @unchecked) match
        case qn: QualifiedName => assgnGen(qn.t, qn, rValue, builder)
        case ArrayElem(qn: QualifiedName, index) => {
          val (dataWidth, pointer) = repeatAccessArray(qn, index, builder)
          builder += MOV(pointer, rValueGen(rValue, builder, qn.t))
        }
        case Fst(lValue2) => {
          fstSndAddress(lValue2, builder) // Store address in R10
          builder += MOV(Reg(9, QWord), Reg(10, QWord)) // Move into R9
          val regOrImm: (Reg | Imm) =
          rValueGen(rValue, builder) match
            case Reg(num, dataWidth) => Reg(num, QWord)
            case Imm(value) => Imm(value)
          builder += MOV(OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(R9, QWord)), regOrImm) // Store RValue in R10, MOV R10 into [R9]
        }
        case Snd(lValue2) => {
          fstSndAddress(lValue2, builder, 8)
          builder += MOV(Reg(9, QWord), Reg(10, QWord))
          val regOrImm: (Reg | Imm) =
          rValueGen(rValue, builder) match
            case Reg(num, dataWidth) => Reg(num, QWord)
            case Imm(value) => Imm(value)
          builder += MOV(OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(R9, QWord)), regOrImm)
        }
  }


  def arrayIndexAccess(pointerAddress:MemAddr, index: Expr, t: Type, builder: Builder[Instr, List[Instr]]): (Int, MemAddr) = {
    builder += MOV(Reg(R9, QWord), pointerAddress)
    val dataWidth = typeToSize(t)
    builder.addAll(
      List(
        MOV(Reg(R8, DWord), exprGen(index, builder)), // Okay because index has to be an int
        CMP(Reg(R8, DWord), Imm(0)),
        JCond(Cond.L, Label("_outOfBounds")),
        CMP(Reg(R8, DWord), OffsetAddr(Some(MemOpModifier.DWordPtr), Reg(R9, QWord), -4)),
        JCond(Cond.GE, Label("_outOfBounds"))
      )
    )
    (dataWidth, RegScale(Some(dataWidth), Reg(R9, QWord), typeToSize(t), Reg(R8, DWord)))
  }

  def rValueGen(rValue: RValue, builder: Builder[Instr, List[Instr]], t: Type = Undefined): (Reg | Imm) = {
    rValue match
      case expr: Expr => exprGen(expr, builder)
      case Fst(lValue) => fstSndAddress(lValue, builder)
      case Snd(lValue) => fstSndAddress(lValue, builder, 8)
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], builder)
      case NewPair(fst, snd) => mallocNewPair(fst, snd, builder)
      case Call(ident, args) => ???
  }

  def storeOnStack(rValue: RValue, dataWidth: Int, offset: Int, builder: Builder[Instr, List[Instr]], t: Type = Undefined) = {
    rValue match
      case expr: Expr => builder += MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), offset), exprGen(expr, builder))
      case Fst(lValue) => fstSndAddress(lValue, builder); builder += MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), offset), Reg(R10, dataWidth))
      case Snd(lValue) => fstSndAddress(lValue, builder, 8); builder += MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), offset), Reg(R10, dataWidth))
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], builder); builder += MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), offset), Reg(Rax, dataWidth))
      case NewPair(fst, snd) => mallocNewPair(fst, snd, builder); builder += MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), offset), Reg(Rax, dataWidth))       
      case Call(ident, args) => ???
  }

  def mallocArrayLiter(elems: List[Expr], arrayType: ArrayType, builder: Builder[Instr, List[Instr]]) = {
    
    val arrayTypeSize = if arrayType.d != 1 then typeToSize(arrayType) else typeToSize(arrayType)
    builder.addAll(
      List(
        MOV(Reg(Rdi, DWord), Imm(arrayTypeSize*(elems.size) + 4)),
        CALL(Label("_malloc")),
        MOV(OffsetAddr(Some(MemOpModifier.DWordPtr), Reg(Rax, QWord)), Imm(elems.size)),
        ADD(Reg(Rax, DWord), Imm(4))
        )
    )
    elems.indices.map(i => {
      builder += MOV(OffsetAddr(Some(arrayTypeSize), Reg(Rax, QWord), i*arrayTypeSize), exprGen(elems(i), builder)) // will be fine
    })
    Reg(Rax, QWord)
  }

  def mallocNewPair(fst: Expr, snd: Expr, builder: Builder[Instr, List[Instr]]) = {
    val (fstSize, sndSize) = (8, 8)
    builder.addAll(
    List(
      MOV(Reg(Rdi, DWord), Imm(fstSize+sndSize)),
      CALL(Label("_malloc")),
      )
    )
    builder += MOV(OffsetAddr(Some(fstSize), Reg(Rax, QWord)), exprGen(fst, builder)) // will be fine
    builder += MOV(OffsetAddr(Some(sndSize), Reg(Rax, QWord), sndSize), exprGen(snd, builder)) // will be fine
    Reg(Rax, QWord)
  }


  def repeatAccessArray(qn: QualifiedName, index: List[Expr], builder: Builder[Instr, List[Instr]]): (Int, MemAddr) = {
    val arrayElemType = qn.t.asInstanceOf[ArrayType]
    var pointer: MemAddr = OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp, QWord), Stack.getOffset(qn))
    for (i <- 1 to index.size - 1) pointer = arrayIndexAccess(pointer, index(i), arrayElemType, builder)._2 
    arrayIndexAccess(pointer, index.last, arrayElemType.t, builder)
  }

  def readFunc(dataWidth: Int,builder: Builder[Instr, List[Instr]]) = {
    builder.addAll(
      List(
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),
        SUB(Reg(Rsp, QWord), Imm(-STACK_ALIGN)),
        MOV(OffsetAddr(Some(dataWidth), Reg(Rsp, QWord)), Reg(Rdi, dataWidth)),
        LEA(Reg(Rsi, QWord), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rsp, QWord))),
      //LEA(Reg(Rsi, QWord), [rip + .L._readi_str0]), lea rdi, [rip + .L._readi_str0],
        MOV(Reg(Rax, Byte), Imm(0)),
        CALL(Label("scanf@plt")),
        MOV(Reg(Rax, dataWidth), OffsetAddr(Some(dataWidth), Reg(Rsp, QWord))),
        ADD(Reg(Rsp, QWord), Imm(-STACK_ALIGN)),
      )
    )
  }

  def readGen(lValue: LValue, builder: Builder[Instr, List[Instr]]) = {
      (lValue: @unchecked) match
        case qn: QualifiedName => {
          val dataWidth = typeToSize(qn.t)
          builder += movQnToReg(Rdi, qn, dataWidth) // Check if bugs later
          // builder += MOV(Reg(Rdi, dataWidth), OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), Stack.getOffset(qn)))
          readFunc(dataWidth, builder)
          builder += movRegOrImmToMem(Rax, qn, dataWidth) // Check if bugs later
          // builder += MOV(OffsetAddr(Some(dataWidth), Reg(Rbp, QWord), Stack.getOffset(qn)), Reg(Rax, dataWidth))
        }
        case ArrayElem(qn: QualifiedName, index) => {
          val arrayBaseType = qn.t.asInstanceOf[ArrayType].t
          val arrayPointer = OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp, QWord), Stack.getOffset(qn))
          val (dataWidth, pointer) =  arrayIndexAccess(arrayPointer, index.last, arrayBaseType, builder)
          builder += MOV(Reg(Rdi, dataWidth), pointer)
          readFunc(dataWidth, builder)
          builder += MOV(pointer, Reg(Rax, dataWidth))
        }
        case Fst(lValue) => readPair(lValue, builder, 0)
    
        case Snd(lValue) => readPair(lValue, builder , 8)
    }
 
  def fstSndAddress(lValue: LValue, builder: Builder[Instr, List[Instr]], fstOrSnd: Int = 0): Reg = {
    val storeInstrs = List(
      CMP(Reg(10, QWord), Imm(0)),
      JCond(Cond.E, Label("_errNull")),
      MOV(Reg(10, QWord), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(R10, QWord), fstOrSnd))
    )
    (lValue: @unchecked) match 
      case qn: QualifiedName => {
        movQnToReg(R10, qn)
        builder.addAll(storeInstrs)
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val (dataWidth, pointer) = repeatAccessArray(qn, index, builder)
        builder += MOV(Reg(10, QWord), pointer)
        builder.addAll(storeInstrs)
      }
      case Fst(lValue) => {
        fstSndAddress(lValue, builder)
        builder.addAll(storeInstrs)
      }
      case Snd(lValue) => {
        fstSndAddress(lValue, builder, 8)
        builder.addAll(storeInstrs)
      }
      Reg(R10, QWord)
    }

  def readPair(lValue: LValue, builder: Builder[Instr, List[Instr]], fstOrSnd: Int) = {
    (lValue: @unchecked) match
      case qn: QualifiedName => {
        val dataWidth = typeToSize(qn.t)
        fstSndAddress(lValue, builder, fstOrSnd)
        builder += MOV(Reg(Rdi, dataWidth), OffsetAddr(Some(dataWidth), Reg(R10, QWord)))
        readFunc(dataWidth, builder)
        builder += MOV(OffsetAddr(Some(dataWidth), Reg(R10, QWord)), Reg(Rax, dataWidth))
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val dataWidth = typeToSize(qn.t.asInstanceOf[ArrayType].t)
        fstSndAddress(lValue, builder, fstOrSnd)
        builder += MOV(Reg(Rdi, dataWidth), OffsetAddr(Some(dataWidth), Reg(R10, QWord)))
        readFunc(dataWidth, builder)
        builder += MOV(OffsetAddr(Some(dataWidth), Reg(R10, QWord)), Reg(Rax, dataWidth))
      }
  }

  def exitGen(expr: Expr, builder: Builder[Instr, List[Instr]]) = {
    builder.addAll(
      List(
        MOV(Reg(Rdi, DWord), exprGen(expr, builder)), // expr can be assumed to be an int
        AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),       // exit can only take an int 
        CALL(Label("exit@plt"))
      )
    )
  }
}