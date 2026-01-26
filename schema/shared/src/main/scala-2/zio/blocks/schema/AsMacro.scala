package zio.blocks.schema

import scala.reflect.macros.blackbox

object AsMacro {
  def materialize[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context): c.Expr[As[A, B]] = {
    import c.universe._

    val tpeA = weakTypeOf[A]
    val tpeB = weakTypeOf[B]

    val intoAB = c.inferImplicitValue(appliedType(weakTypeOf[Into[_, _]].typeConstructor, tpeA, tpeB))
    val intoBA = c.inferImplicitValue(appliedType(weakTypeOf[Into[_, _]].typeConstructor, tpeB, tpeA))

    val treeAB = if (intoAB != EmptyTree) intoAB else q"implicitly[zio.blocks.schema.Into[$tpeA, $tpeB]]"
    val treeBA = if (intoBA != EmptyTree) intoBA else q"implicitly[zio.blocks.schema.Into[$tpeB, $tpeA]]"

    c.Expr[As[A, B]](q"""
      new zio.blocks.schema.As[$tpeA, $tpeB] {
        def into(input: $tpeA): Either[zio.blocks.schema.SchemaError, $tpeB] = $treeAB.into(input)
        def from(input: $tpeB): Either[zio.blocks.schema.SchemaError, $tpeA] = $treeBA.into(input)
      }
    """)
  }
}
