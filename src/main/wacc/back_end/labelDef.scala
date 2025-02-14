package wacc.back_end

import scala.collection.mutable.{Builder}

sealed trait LabelDef

case class FuncLabelDef(
  name: String,
  instructions: Builder[Instr, List[Instr]],
  localLabelDefs: Builder[LocalLabelDef, List[LocalLabelDef]]) extends LabelDef

case class LocalLabelDef(
  name: String,
  instructions: Builder[Instr, List[Instr]],
) extends LabelDef
