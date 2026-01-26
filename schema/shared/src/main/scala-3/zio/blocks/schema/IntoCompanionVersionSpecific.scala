package zio.blocks.schema

import scala.deriving.Mirror

trait IntoCompanionVersionSpecific extends IntoLowPriority {
  inline given derive[A, B](using Mirror.Of[A], Mirror.Of[B]): Into[A, B] = ${ IntoMacro.derive[A, B] }
}
