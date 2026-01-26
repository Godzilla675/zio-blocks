package zio.blocks.schema

import scala.deriving.Mirror

trait AsCompanionVersionSpecific {
  inline given derive[A, B](using Mirror.Of[A], Mirror.Of[B]): As[A, B] = ${ AsMacro.derive[A, B] }
}
