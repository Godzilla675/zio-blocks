package zio.blocks.schema

import scala.language.experimental.macros

trait IntoCompanionVersionSpecific {
  implicit def materialize[A, B]: Into[A, B] = macro IntoMacro.materialize[A, B]
}
