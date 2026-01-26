package zio.blocks.schema

import scala.language.experimental.macros

trait AsCompanionVersionSpecific {
  implicit def materialize[A, B]: As[A, B] = macro AsMacro.materialize[A, B]
}
