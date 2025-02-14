package wacc.back_end

// At the start of each assembly file
// .intel_syntax noprefix
// .globl main
object IR {
  val sections = List.newBuilder[Section] // each section is .rodata (read only)
                                          // what follows is .text
  val labelDefs = List.newBuilder[LabelDef] // each function may contain labels/jumping points (ie for loops)

}