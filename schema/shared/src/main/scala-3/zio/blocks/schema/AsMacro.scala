package zio.blocks.schema

import scala.quoted._

object AsMacro {
  def derive[A: Type, B: Type](using Quotes): Expr[As[A, B]] = {
    val intoAB = IntoMacro.derive[A, B]
    val intoBA = IntoMacro.derive[B, A]

    '{
      new As[A, B] {
        def into(input: A): Either[SchemaError, B] = $intoAB.into(input)
        def from(input: B): Either[SchemaError, A] = $intoBA.into(input)
      }
    }
  }
}
