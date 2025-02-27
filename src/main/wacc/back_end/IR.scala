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
    case Assgn(t, identifier: QualifiedName, rValue) => assgnGen(t, identifier, rValue, builder)
    case ReAssgn(lValue, rValue) => reassgnGen(lValue, rValue, builder)
    case Exit(expr) => {
      builder.addAll(
        List(
          MOV(Reg(Rdi, DWord), exprGen(expr, builder)), // expr can be assumed to be an int
          AND(Reg(Rsp, QWord), Imm(STACK_ALIGN)),       // exit can only take an int 
          CALL(Label("exit@plt"))
        )
      )
    }
    case Read(lValue) => ???
    case Free(expr) => ???
    case Return(expr) => ???
    case Scope(stmts) => ???
    case WhileDo(condition, stmts) => ???
    case IfElse(condition, thenStmts, elseStmts) => ???
    case Print(expr) => ???
    case Println(expr) => ???
  }

  def assgnGen(t: Type, identifier: QualifiedName, rValue: RValue, builder: Builder[Instr, List[Instr]]) : Builder[Instr, List[Instr]] = {
    val offset = Stack.frames.last.identTable(identifier)
    val memSize = Stack.typeToSize(t)
    rValueGen(rValue, builder, t)
    storeOnStack(rValue, memSize, offset, builder)
  }
  
  def reassgnGen(lValue: LValue, rValue: RValue, builder: Builder[Instr, List[Instr]]) = {
    lValue match
        case qn: QualifiedName => assgnGen(qn.t, qn, rValue, builder)
        case ArrayElem(qn: QualifiedName, index) => {
          val (memSize, pointer) = repeatAccessArray(qn, index, builder)
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
    val memSize = typeToSize(t)
    builder.addAll(
      List(
        MOV(Reg(R8, DWord), exprGen(index, builder)), // Okay because index has to be an int
        CMP(Reg(R8, DWord), Imm(0)),
        //JL OUT OF BOUNDS
        CMP(Reg(R8, DWord), OffsetAddr(Some(MemOpModifier.DWordPtr), Reg(R9, QWord), -4)))
        //JGE OUT OF BOUNDS
    )
    (memSize, RegScale(Some(memSize), Reg(R9, QWord), typeToSize(t), Reg(R8, DWord)))
  }

  def rValueGen(rValue: RValue, builder: Builder[Instr, List[Instr]], t: Type = Undefined): (Reg | Imm) = {
    rValue match
      case expr: Expr => exprGen(expr, builder) // will be fine
      case Fst(lValue) => fstSndAddress(lValue, builder)
      case Snd(lValue) => fstSndAddress(lValue, builder, 8)
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType], builder)
      case NewPair(fst, snd) => mallocNewPair(fst, snd, builder)
      case Call(ident, args) => ???
  }

  def storeOnStack(rValue: RValue, memSize: Int, offset: Int, builder: Builder[Instr, List[Instr]]) = {
    rValue match
      case expr: Expr => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp, QWord), offset), Reg(10, memSize))
      case Fst(lValue) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp, QWord), offset), Reg(R10, memSize))
      case Snd(lValue) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp, QWord), offset), Reg(R10, memSize))
      case ArrayLiter(elems) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp, QWord), offset), Reg(Rax, memSize))
      case NewPair(fst, snd) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp, QWord), offset), Reg(Rax, memSize))       
      case Call(ident, args) => ???
  }

  def mallocArrayLiter(elems: List[Expr], arrayType: ArrayType, builder: Builder[Instr, List[Instr]]) = {
    val arrayTypeSize = typeToSize(arrayType)
    if arrayType.d != 1 then { val arrayTypeSize = typeToSize(arrayType) }
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
    var pointer: MemAddr = OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp, QWord), Stack.frames.last.identTable(qn))
    var i = 1
    while ((index.size - i) > 0) { pointer = arrayIndexAccess(pointer, index(i), arrayElemType, builder)._2 }
    arrayIndexAccess(pointer, index.last, arrayElemType.t, builder)
  }






  def fstSndAddress(lValue: LValue, builder: Builder[Instr, List[Instr]], fstOrSnd: Int = 0): Reg = {
    val storeInstrs = List(
      CMP(Reg(10, QWord), Imm(0)),
      JCond(Cond.E, Label("_errNull")),
      MOV(Reg(10, QWord), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(R10, QWord), fstOrSnd))
    )
    lValue match 
      case qn: QualifiedName => {
        val pairOffset = Stack.frames.last.identTable(qn)
        builder += MOV(Reg(10, QWord), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp, QWord), pairOffset))
        builder.addAll(storeInstrs)
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val (memSize, pointer) = repeatAccessArray(qn, index, builder)
        builder += MOV(Reg(10, memSize), pointer)
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
  }

