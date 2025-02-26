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
    ???
    // This will generate the boilerplate at the beginning of the asm file and
    // the string+len of string stuff to go in that section
  }

  def generate(prog: Prog) = prog match {
    case Prog(_, main) =>
      mainGenerate(main)
  }

  def mainGenerate(stmts: List[Stmt]): Builder[Instr, List[Instr]] = {
    val instructionBuilder = List.newBuilder[Instr]
    Stack.initialise(stmts)
    println(Stack.frames.last.identTable)
    stmts.map(stmtGen(_, instructionBuilder))
    return instructionBuilder
  }

  def stmtGen(stmt: Stmt, builder: Builder[Instr, List[Instr]]) = stmt match {
    case Skip => Nil
    case Read(lValue) => ???
    case Free(expr) => ???
    case Return(expr) => ???
    case Exit(expr) => {
      exprGen(expr, builder)
      builder.addAll(
        List(
          MOV(Reg(Rdi, DWord), Reg(R10)),
          AND(Reg(Rsp), Imm(STACK_ALIGN)),
          CALL(Label("exit@plt"))
        )
      )
    }
    case Print(expr) => ???
    case Println(expr) => ???
    case WhileDo(condition, stmts) => ???
    case IfElse(condition, thenStmts, elseStmts) => ???
    case Assgn(t, identifier: QualifiedName, rValue) => assgnGen(t, identifier, rValue, builder)
    case ReAssgn(lValue, rValue) => reassgnGen(lValue, rValue, builder)
    case Scope(stmts) => ???
  }

  def assgnGen(t: Type, identifier: QualifiedName, rValue: RValue, builder: Builder[Instr, List[Instr]]) : Builder[Instr, List[Instr]] = {
    val offset = Stack.frames.last.identTable(identifier)
    val memSize = Stack.typeToSize(t)

    rValue match
      case expr: Expr => {
        exprGen(expr, builder)
        builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(10))
      }

      case ArrayLiter(elems) => {
        val arrayTypeSize = typeToSize(t.asInstanceOf[ArrayType].t)
        builder.addAll(
          List(
            MOV(Reg(Rdi), Imm(arrayTypeSize*(elems.size) + 4)),
            CALL(Label("_malloc")),
            MOV(OffsetAddr(Some(memSize), Reg(Rax)), Imm(elems.size)),
            ADD(Reg(Rax), Imm(4))
            )
        )
        elems.indices.map(i => {
          exprGen(elems(i), builder)
          builder += MOV(OffsetAddr(Some(arrayTypeSize), Reg(Rax), i*arrayTypeSize), Reg(R10))
        })
        builder += MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(Rax))
      }

      case NewPair(fst, snd) => {
        // Need to deal with the case fst or snd are a pair
        val (fstSize, sndSize) = (8, 8)
        builder.addAll(
          List(
            MOV(Reg(Rdi), Imm(fstSize+sndSize)),
            CALL(Label("_malloc")),
            MOV(OffsetAddr(Some(memSize), Reg(Rbp), offset), Reg(Rax))
            )
        )
          exprGen(fst, builder)
          builder += MOV(OffsetAddr(Some(fstSize), Reg(Rax)),Reg(R10))
          exprGen(snd, builder)
          builder += MOV(OffsetAddr(Some(sndSize), Reg(Rax), sndSize),Reg(R10))
        
      } 


      case Fst(lValue) => {
        fstSndHandle(lValue, builder)
        builder += MOV(OffsetAddr(Some(8), Reg(Rbp), 8), Reg(R9))
      }
      case Snd(lValue) => {
        fstSndHandle(lValue, builder, 8)
        builder += MOV(OffsetAddr(Some(8), Reg(Rbp), 8), Reg(R9))
      }
      case Call(ident, args) => ???
  }
  
  def reassgnGen(lValue: LValue, rValue: RValue, builder: Builder[Instr, List[Instr]]) = {
    lValue match
        case qn: QualifiedName => assgnGen(qn.t, qn, rValue, builder)
        case ArrayElem(arrayName, index) => ???
        case Fst(lValue) => ???
        case Snd(lValue) => ???
  }

  def fstSndHandle(lValue: LValue, builder: Builder[Instr, List[Instr]], fstOrSnd: Int = 0): List[Instr] = {
    lValue match 
      case qn: QualifiedName => {
        val pairOffset = Stack.frames.last.identTable(qn)
        List(
        MOV(Reg(9), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp), pairOffset)),
        CMP(Reg(9), Imm(0)),
        JCond(Cond.E, Label("_errNull")),
        MOV(Reg(9), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(Rbp), pairOffset + fstOrSnd)))
      }
      case ArrayElem(arrayName, index) => ???
      case Fst(lValue) => {
        fstSndHandle(lValue, builder) ++
        List(
        CMP(Reg(9), Imm(0)),
        JCond(Cond.E, Label("_errNull")),
        MOV(Reg(9), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(9), fstOrSnd))
        )
      }
      case Snd(lValue) => {
        fstSndHandle(lValue, builder, 8) ++
        List(
        CMP(Reg(9), Imm(0)),
        JCond(Cond.E, Label("_errNull")),
        MOV(Reg(9), OffsetAddr(Some(MemOpModifier.QWordPtr), Reg(9), fstOrSnd))
        )
      }
  }
}

