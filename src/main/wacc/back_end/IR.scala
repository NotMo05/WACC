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
    instructionBuilder += PUSH(Reg(Rbp))
    instructionBuilder += MOV(Reg(Rbp), Reg(Rsp))
    Stack.initialise(stmts)
    instructionBuilder += SUB(Reg(Rsp), Imm(-Stack.frames.last.currentDepth))
    println(Stack.frames.last.identTable)
    stmts.map(stmtGen(_, instructionBuilder))
    return LocalLabelDef("main", instructionBuilder)
  }

  def stmtGen(stmt: Stmt, builder: Builder[Instr, List[Instr]]) = stmt match {
    case Skip => Nil
    case Assgn(t, identifier: QualifiedName, rValue) => assgnGen(t, identifier, rValue, builder)
    case ReAssgn(lValue, rValue) => reassgnGen(lValue, rValue, builder)
    case Exit(expr) => {
      builder.addAll(
        List(
          MOV(Reg(Rdi, DWord), exprGen(expr, builder)),
          AND(Reg(Rsp), Imm(STACK_ALIGN)),
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
          val pointer = repeatAccessArray(qn, index, builder)
          builder += MOV(pointer, rValueGen(rValue, builder, qn.t))
        }
        case Fst(lValue2) => {
          fstSndAddress(lValue2, builder)
          builder += MOV(Reg(9), Reg(10))
          builder += MOV(OffsetAddr(Some(8), Reg(R9)), rValueGen(rValue, builder))
        }
        case Snd(lValue2) => {
          fstSndAddress(lValue2, builder, 8)
          builder += MOV(Reg(9), Reg(10))
          builder += MOV(OffsetAddr(Some(8), Reg(R9)), rValueGen(rValue, builder))
        }
  }


  def arrayIndexAccess(pointerAddress:MemAddr, index: Expr, t: Type, builder: Builder[Instr, List[Instr]]): MemAddr = {
    builder += MOV(Reg(R9), pointerAddress)
    builder.addAll(
      List(
        MOV(Reg(R8), exprGen(index, builder)),
        CMP(Reg(R8), Imm(0)),
        //JL OUT OF BOUNDS
        CMP(Reg(R8), OffsetAddr(Some(8), Reg(R9), -4)))
        //JGE OUT OF BOUNDS
    )
    RegScale(Some(8), Reg(R9), typeToSize(t), Reg(R8))
  }

  def rValueGen(rValue: RValue, builder: Builder[Instr, List[Instr]], t: Type = Undefined) = {
    rValue match
      case expr: Expr => exprGen(expr, builder)
      case Fst(lValue) => fstSndAddress(lValue, builder)
      case Snd(lValue) => fstSndAddress(lValue, builder, 8)
      case ArrayLiter(elems) => mallocArrayLiter(elems, t.asInstanceOf[ArrayType].t, builder)
      case NewPair(fst, snd) => mallocNewPair(fst, snd, builder)
      case Call(ident, args) => ???
  }

  def storeOnStack(rValue: RValue, memSize: Int, offset: Int, builder: Builder[Instr, List[Instr]]) = {
    rValue match
      case expr: Expr => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(10))
      case Fst(lValue) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(R10))
      case Snd(lValue) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(R10))
      case ArrayLiter(elems) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(Rax))
      case NewPair(fst, snd) => builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(Rax))       
      case Call(ident, args) => ???
  }

  def mallocArrayLiter(elems: List[Expr], t: Type, builder: Builder[Instr, List[Instr]]) = {
    val arrayTypeSize = typeToSize(t)
    builder.addAll(
      List(
        MOV(Reg(Rdi), Imm(arrayTypeSize*(elems.size) + 4)),
        CALL(Label("_malloc")),
        MOV(OffsetAddr(Some(4), Reg(Rax)), Imm(elems.size)),
        ADD(Reg(Rax), Imm(4))
        )
    )
    elems.indices.map(i => {
      builder += MOV(OffsetAddr(Some(arrayTypeSize), Reg(Rax), i*arrayTypeSize), exprGen(elems(i), builder))
    })
    Reg(Rax)
  }

  def mallocNewPair(fst: Expr, snd: Expr, builder: Builder[Instr, List[Instr]]) = {
    val (fstSize, sndSize) = (8, 8)
    builder.addAll(
    List(
      MOV(Reg(Rdi), Imm(fstSize+sndSize)),
      CALL(Label("_malloc")),
      )
    )
    builder += MOV(OffsetAddr(Some(fstSize), Reg(Rax)), exprGen(fst, builder))
    builder += MOV(OffsetAddr(Some(sndSize), Reg(Rax), sndSize), exprGen(snd, builder))
    Reg(Rax)
  }


  def repeatAccessArray(qn: QualifiedName, index: List[Expr], builder: Builder[Instr, List[Instr]]): MemAddr = {
    val arrayElemType = qn.t.asInstanceOf[ArrayType]
    var pointer: MemAddr = OffsetAddr(Some(8), Reg(Rbp), Stack.frames.last.identTable(qn))
    var i = 1
    while ((index.size - i) > 0) pointer = arrayIndexAccess(pointer, index(i), arrayElemType, builder)
    arrayIndexAccess(pointer, index.last, arrayElemType.t, builder)
  }






  def fstSndAddress(lValue: LValue, builder: Builder[Instr, List[Instr]], fstOrSnd: Int = 0): Reg = {
    val storeInstrs = List(
      CMP(Reg(10), Imm(0)),
      JCond(Cond.E, Label("_errNull")),
      MOV(Reg(10), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(R10), fstOrSnd))
    )
    lValue match 
      case qn: QualifiedName => {
        val pairOffset = Stack.frames.last.identTable(qn)
        builder += MOV(Reg(10), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp), pairOffset))
        builder.addAll(storeInstrs)
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val pointer = repeatAccessArray(qn, index, builder)
        builder += MOV(Reg(10), pointer)
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
      Reg(R10)
    }
  }

