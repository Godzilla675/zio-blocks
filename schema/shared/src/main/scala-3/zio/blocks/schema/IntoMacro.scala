package zio.blocks.schema

import scala.quoted._

object IntoMacro {
  def derive[A: Type, B: Type](using Quotes): Expr[Into[A, B]] = {
    import quotes.reflect._

    val tpeA = TypeRepr.of[A].dealias
    val tpeB = TypeRepr.of[B].dealias

    // report.info(s"DEBUG: Deriving Into[${tpeA.show}, ${tpeB.show}]")

    val symA = tpeA.typeSymbol
    val symB = tpeB.typeSymbol

    def deriveProduct(tpeA: TypeRepr, tpeB: TypeRepr): Expr[Into[A, B]] = {
      // Filter distinct names because Tuples in Scala 3 might return duplicate fields (e.g. _1 and _1 )
      val fieldsA = symA.caseFields.distinctBy(_.name.trim)
      val fieldsB = symB.caseFields.distinctBy(_.name.trim)

      def findMatch(fieldB: Symbol, indexB: Int): Option[(Symbol, TypeRepr)] = {
        val name = fieldB.name
        // val tpeFieldB = tpeB.memberType(fieldB)

        // 1. Exact name
        fieldsA.find(_.name == name).map { fieldA =>
          (fieldA, tpeA.memberType(fieldA))
        }.orElse {
          // 3. Positional fallback
          if (fieldsA.size > indexB) {
             val fieldA = fieldsA(indexB)
             Some((fieldA, tpeA.memberType(fieldA)))
          } else {
             None
          }
        }
      }

      val fn = Lambda(
        Symbol.spliceOwner,
        MethodType(List("input"))(_ => List(tpeA), _ => TypeRepr.of[Either[SchemaError, B]]),
        (methodSym, params) => {
          val inputTerm = params.head.asInstanceOf[Term]

          val validations = fieldsB.zipWithIndex.map { case (fieldB, idx) =>
            val name = fieldB.name
            val tpeFieldB = tpeB.memberType(fieldB)

            val resultExpr: Expr[Either[SchemaError, Any]] = findMatch(fieldB, idx) match {
              case Some((fieldA, tpeFieldA)) =>
                tpeFieldA.widen.asType match { case '[tA] =>
                  val valueA = Select(inputTerm, fieldA).asExprOf[tA]
                  tpeFieldB.widen.asType match { case '[tB] =>
                    val into = Expr.summon[Into[tA, tB]].getOrElse {
                      report.errorAndAbort(s"Cannot find Into[${TypeRepr.of[tA].show}, ${TypeRepr.of[tB].show}] for field '$name' (mapped from ${fieldA.name} at idx $idx)")
                    }
                    '{ $into.into($valueA).map(_.asInstanceOf[Any]) }
                  }
                }
              case None =>
                val idx1 = idx + 1
                val companion = symB.companionModule

                // Try apply$default$N, then $lessinit$greater$default$N
                val defaultSym = companion.methodMember(s"apply$$default$$$idx1")
                  .headOption
                  .orElse(companion.methodMember(s"$$lessinit$$greater$$default$$$idx1").headOption)

                if (defaultSym.nonEmpty) {
                  val defaultCall = Select(Ref(companion), defaultSym.head).asExpr
                  '{ Right($defaultCall).map(_.asInstanceOf[Any]) }
                } else if (tpeFieldB <:< TypeRepr.of[Option[?]]) {
                  '{ Right(None).map(_.asInstanceOf[Any]) }
                } else {
                  report.errorAndAbort(s"Cannot find value for field '$name' in source ${tpeA.show} and no default value found")
                }
            }
            (name, resultExpr, tpeFieldB)
          }

          val resultDefinitions = validations.zipWithIndex.map { case ((_, expr, _), i) =>
            val valSym = Symbol.newVal(methodSym, s"r_$i", TypeRepr.of[Either[SchemaError, Any]], Flags.EmptyFlags, Symbol.noSymbol)
            ValDef(valSym, Some(expr.asTerm.changeOwner(valSym)))
          }

          val resultRefs = resultDefinitions.map(vd => Ref(vd.symbol))

          // Calculate errors - Properly accumulate errors and attach field info
          val errorsVar = Symbol.newVal(methodSym, "errors", TypeRepr.of[List[SchemaError.Single]], Flags.Mutable, Symbol.noSymbol)
          val errorsValDef = ValDef(errorsVar, Some('{ List.empty[SchemaError.Single] }.asTerm))
          val errorsRef = Ref(errorsVar)

          val valueDefinitions = validations.zipWithIndex.map { case ((name, _, _), i) =>
            val rRef = resultRefs(i)

            val isLeft = Select(rRef, rRef.tpe.typeSymbol.methodMember("isLeft").head)

            // Branch for Left (failure)
            val ifLeft = {
              // Construct the expression to calculate new error list
              val newErrorsList = '{
                 val e = ${Select(Select(rRef, rRef.tpe.typeSymbol.methodMember("left").head), Symbol.requiredClass("scala.util.Either.LeftProjection").methodMember("get").head).asExprOf[SchemaError]}
                 val enriched = e.errors.map { s =>
                    val newSource = DynamicOptic(DynamicOptic.Node.Field(${Expr(name)}) +: s.source.nodes)
                    s match {
                      case x: SchemaError.MissingField => x.copy(source = newSource)
                      case x: SchemaError.DuplicatedField => x.copy(source = newSource)
                      case x: SchemaError.ExpectationMismatch => x.copy(source = newSource)
                      case x: SchemaError.UnknownCase => x.copy(source = newSource)
                      case x: SchemaError.ValidationFailed => x.copy(source = newSource)
                    }
                 }
                 // Prepend enriched errors to existing errors
                 enriched ::: ${errorsRef.asExprOf[List[SchemaError.Single]]}
              }

              val assign = Assign(errorsRef, newErrorsList.asTerm)
              Block(List(assign), '{ null }.asTerm)
            }

            // Branch for Right (success)
            val ifRight = {
               Select(Select(rRef, rRef.tpe.typeSymbol.methodMember("right").head), Symbol.requiredClass("scala.util.Either.RightProjection").methodMember("get").head)
            }

            val valueTerm = If(isLeft, ifLeft, ifRight)

            val valSym = Symbol.newVal(methodSym, s"v_$i", TypeRepr.of[Any], Flags.EmptyFlags, Symbol.noSymbol)
            ValDef(valSym, Some(valueTerm.changeOwner(valSym)))
          }

          val valuesRefs = valueDefinitions.map(vd => Ref(vd.symbol))

          val args = valuesRefs.zipWithIndex.map { case (ref, i) =>
             val t = validations(i)._3
             t.asType match { case '[tt] =>
               '{ ${ref.asExprOf[Any]}.asInstanceOf[tt] }.asTerm
             }
          }

          val constructor = Select(Ref(symB.companionModule), symB.companionModule.methodMember("apply").head)
          val newB = Apply(constructor, args.toList)

          val checkErrors = '{
             if (${errorsRef.asExprOf[List[SchemaError.Single]]}.isEmpty) {
               Right(${newB.asExprOf[B]})
             } else {
               Left(SchemaError(${errorsRef.asExprOf[List[SchemaError.Single]]}.reverse.to(List).asInstanceOf[::[SchemaError.Single]]))
             }
          }.asTerm

          Block(resultDefinitions ++ List(errorsValDef) ++ valueDefinitions, checkErrors)
        }
      )

      val fnExpr = fn.asExprOf[A => Either[SchemaError, B]]
      '{
         new Into[A, B] {
            def into(input: A): Either[SchemaError, B] = $fnExpr(input)
         }
      }
    }

    def deriveCoproduct(tpeA: TypeRepr, tpeB: TypeRepr): Expr[Into[A, B]] = {
      val subTypesA = CommonMacroOps.directSubTypes(tpeA)
      val subTypesB = CommonMacroOps.directSubTypes(tpeB)

      val fn = Lambda(
        Symbol.spliceOwner,
        MethodType(List("input"))(_ => List(tpeA), _ => TypeRepr.of[Either[SchemaError, B]]),
        (methodSym, params) => {
          val inputTerm = params.head.asInstanceOf[Term]

          val matchCases = subTypesA.zipWithIndex.map { case (tpeCaseA, idx) =>
            val symCaseA = tpeCaseA.typeSymbol

            def findMatch: Option[TypeRepr] = {
              subTypesB.find(_.typeSymbol.name == symCaseA.name)
            }

            val bindName = s"x_$idx" // Unique name for each case bind
            val bindSymbol = Symbol.newBind(methodSym, bindName, Flags.EmptyFlags, tpeCaseA)
            val bindPattern = Bind(bindSymbol, Typed(Wildcard(), TypeTree.of(using tpeCaseA.asType)))

            val body = findMatch match {
              case Some(tpeCaseB) =>
                tpeCaseA.asType match { case '[tCaseA] =>
                  tpeCaseB.asType match { case '[tCaseB] =>
                    val into = Expr.summon[Into[tCaseA, tCaseB]].getOrElse {
                      report.errorAndAbort(s"Cannot find Into[${TypeRepr.of[tCaseA].show}, ${TypeRepr.of[tCaseB].show}] for case '${symCaseA.name}'")
                    }
                    '{ $into.into(${Ref(bindSymbol).asExpr.asInstanceOf[Expr[tCaseA]]}) }
                  }
                }
              case None =>
                '{ Left(SchemaError.unknownCase(Nil, ${Expr(symCaseA.name)})) }
            }

            CaseDef(bindPattern, None, body.asTerm.changeOwner(methodSym))
          }

          Match(inputTerm, matchCases)
        }
      )

      val fnExpr = fn.asExprOf[A => Either[SchemaError, B]]
      '{
         new Into[A, B] {
            def into(input: A): Either[SchemaError, B] = $fnExpr(input)
         }
      }
    }

    if (symB.flags.is(Flags.Module)) {
       // Case object or module
       '{
          val b = ${Ref(symB.companionModule).asExprOf[B]}
          new Into[A, B] {
             def into(input: A): Either[SchemaError, B] = Right(b)
          }
       }
    } else if (symB.flags.is(Flags.Case)) {
      deriveProduct(tpeA, tpeB)
    } else if (symB.flags.is(Flags.Sealed) || symB.flags.is(Flags.Enum)) {
      deriveCoproduct(tpeA, tpeB)
    } else {
      report.errorAndAbort(s"Cannot derive Into[${tpeA.show}, ${tpeB.show}]: Target is not a case class or sealed trait")
    }
  }
}
