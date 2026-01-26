package zio.blocks.schema

import scala.reflect.macros.blackbox

object IntoMacro {
  def materialize[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context): c.Expr[Into[A, B]] = {
    import c.universe._

    val tpeA = weakTypeOf[A].dealias
    val tpeB = weakTypeOf[B].dealias

    val symB = tpeB.typeSymbol

    if (symB.isClass && symB.asClass.isCaseClass) {
      materializeProduct(c)(tpeA, tpeB)
    } else if (symB.isClass && (symB.asClass.isSealed || symB.asClass.isTrait)) {
      materializeCoproduct(c)(tpeA, tpeB)
    } else {
      c.abort(c.enclosingPosition, s"Target type $tpeB is not a case class or sealed trait")
    }
  }

  private def materializeProduct[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(tpeA: c.Type, tpeB: c.Type): c.Expr[Into[A, B]] = {
    import c.universe._

    val fieldsA = tpeA.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m }.toList
    val fieldsB = tpeB.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m }.toList

    val validations = fieldsB.zipWithIndex.map { case (fieldB, idx) =>
      val name = fieldB.name.decodedName.toString
      val tpeFieldB = tpeB.memberType(fieldB).finalResultType

      val matchA = fieldsA.find(_.name.decodedName.toString == name).map { fieldA =>
        (fieldA, tpeA.memberType(fieldA).finalResultType)
      }.orElse {
        val candidates = fieldsA.filter { fieldA =>
           val tpeFieldA = tpeA.memberType(fieldA).finalResultType
           tpeFieldA <:< tpeFieldB
        }
        if (candidates.size == 1) Some((candidates.head, tpeA.memberType(candidates.head).finalResultType))
        else None
      }

      val resultTree = matchA match {
        case Some((fieldA, tpeFieldA)) =>
          val into = c.inferImplicitValue(appliedType(weakTypeOf[Into[_, _]].typeConstructor, tpeFieldA, tpeFieldB))
          if (into == EmptyTree) {
            c.abort(c.enclosingPosition, s"Cannot find Into[$tpeFieldA, $tpeFieldB] for field $name")
          }
          q"$into.into(input.$fieldA)"

        case None =>
          val companion = tpeB.typeSymbol.companion
          val defaultMethodName = TermName("apply$default$" + (idx + 1))
          val defaultMethod = companion.typeSignature.member(defaultMethodName)

          if (defaultMethod != NoSymbol) {
             q"Right($companion.$defaultMethodName)"
          } else if (tpeFieldB <:< typeOf[Option[_]]) {
             q"Right(None)"
          } else {
             c.abort(c.enclosingPosition, s"Cannot find value for field $name in source $tpeA and no default value found")
          }
      }
      (name, resultTree, tpeFieldB)
    }

    val body = {
      val resultNames = validations.zipWithIndex.map { case (_, i) => TermName(s"r_$i") }

      val resultDefs = validations.zip(resultNames).map { case ((name, tree, _), resultName) =>
        q"val $resultName = $tree"
      }

      val errorsTerm = TermName("errors")
      val errorsDef = q"var $errorsTerm: List[zio.blocks.schema.SchemaError.Single] = Nil"

      val valueNames = validations.zipWithIndex.map { case (_, i) => TermName(s"v_$i") }

      val valueDefs = validations.zip(resultNames).zip(valueNames).map { case (((name, _, tpeFieldB), resultName), valueName) =>
        val leftBranch = q"""
           val e = $resultName.left.get
           val newErrors = e.errors.map { s =>
              val newSource = zio.blocks.schema.DynamicOptic(zio.blocks.schema.DynamicOptic.Node.Field($name) +: s.source.nodes)
               s match {
                  case x: zio.blocks.schema.SchemaError.MissingField => x.copy(source = newSource)
                  case x: zio.blocks.schema.SchemaError.DuplicatedField => x.copy(source = newSource)
                  case x: zio.blocks.schema.SchemaError.ExpectationMismatch => x.copy(source = newSource)
                  case x: zio.blocks.schema.SchemaError.UnknownCase => x.copy(source = newSource)
                  case x: zio.blocks.schema.SchemaError.ValidationFailed => x.copy(source = newSource)
               }
           }
           $errorsTerm = newErrors ::: $errorsTerm
           null.asInstanceOf[$tpeFieldB]
        """

        q"""
           val $valueName: $tpeFieldB = if ($resultName.isLeft) {
              $leftBranch
           } else {
              $resultName.right.get
           }
        """
      }

      val constructor = q"new $tpeB(..$valueNames)"

      q"""
         ..$resultDefs
         $errorsDef
         ..$valueDefs

         if ($errorsTerm.isEmpty) Right($constructor)
         else Left(zio.blocks.schema.SchemaError($errorsTerm.reverse.asInstanceOf[::[zio.blocks.schema.SchemaError.Single]]))
       """
    }

    c.Expr[Into[A, B]](q"""
      new zio.blocks.schema.Into[$tpeA, $tpeB] {
        def into(input: $tpeA): Either[zio.blocks.schema.SchemaError, $tpeB] = {
           $body
        }
      }
    """)
  }

  private def materializeCoproduct[A: c.WeakTypeTag, B: c.WeakTypeTag](c: blackbox.Context)(tpeA: c.Type, tpeB: c.Type): c.Expr[Into[A, B]] = {
    import c.universe._

    val subTypesA = CommonMacroOps.directSubTypes(c)(tpeA)
    val subTypesB = CommonMacroOps.directSubTypes(c)(tpeB)

    val cases = subTypesA.map { tpeCaseA =>
      val symCaseA = tpeCaseA.typeSymbol
      val nameA = symCaseA.name.decodedName.toString

      val matchB = subTypesB.find(_.typeSymbol.name.decodedName.toString == nameA)

      val body = matchB match {
        case Some(tpeCaseB) =>
          val into = c.inferImplicitValue(appliedType(weakTypeOf[Into[_, _]].typeConstructor, tpeCaseA, tpeCaseB))
          if (into == EmptyTree) {
            c.abort(c.enclosingPosition, s"Cannot find Into[$tpeCaseA, $tpeCaseB] for case $nameA")
          }
          q"$into.into(x)"
        case None =>
          q"Left(zio.blocks.schema.SchemaError.unknownCase(Nil, $nameA))"
      }

      cq"x: $tpeCaseA => $body"
    }

    c.Expr[Into[A, B]](q"""
      new zio.blocks.schema.Into[$tpeA, $tpeB] {
         def into(input: $tpeA): Either[zio.blocks.schema.SchemaError, $tpeB] = {
            input match {
               case ..$cases
            }
         }
      }
    """)
  }
}
