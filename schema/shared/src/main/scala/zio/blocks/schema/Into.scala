package zio.blocks.schema

import scala.collection.Factory

trait Into[-A, +B] {
  def into(input: A): Either[SchemaError, B]
}

object Into extends IntoCompanionVersionSpecific {
  def apply[A, B](implicit into: Into[A, B]): Into[A, B] = into

  // Primitive conversions
  implicit val intIntoLong: Into[Int, Long] = i => Right(i.toLong)
  implicit val intIntoDouble: Into[Int, Double] = i => Right(i.toDouble)
  implicit val intIntoFloat: Into[Int, Float] = i => Right(i.toFloat)
  implicit val longIntoDouble: Into[Long, Double] = i => Right(i.toDouble)
  implicit val floatIntoDouble: Into[Float, Double] = i => Right(i.toDouble)

  // Narrowing with validation
  implicit val longIntoInt: Into[Long, Int] = i =>
    if (i >= Int.MinValue && i <= Int.MaxValue) Right(i.toInt)
    else Left(SchemaError.validationFailed(Nil, s"Value $i out of range for Int"))

  implicit val doubleIntoInt: Into[Double, Int] = i =>
    if (i >= Int.MinValue && i <= Int.MaxValue && i == i.toInt.toDouble) Right(i.toInt)
    else Left(SchemaError.validationFailed(Nil, s"Value $i out of range/precision for Int"))

  implicit val doubleIntoLong: Into[Double, Long] = i =>
    if (i >= Long.MinValue && i <= Long.MaxValue && i == i.toLong.toDouble) Right(i.toLong)
    else Left(SchemaError.validationFailed(Nil, s"Value $i out of range/precision for Long"))

  // Identity
  implicit def identityInto[A]: Into[A, A] = a => Right(a)

  implicit def optionInto[A, B](implicit i: Into[A, B]): Into[Option[A], Option[B]] = new Into[Option[A], Option[B]] {
    def into(input: Option[A]): Either[SchemaError, Option[B]] = input match {
      case Some(a) => i.into(a).map(Some(_))
      case None => Right(None)
    }
  }

  implicit def iterableInto[A, B, C1[X] <: Iterable[X], C2[X] <: Iterable[X]](implicit
      elementInto: Into[A, B],
      factory: Factory[B, C2[B]]
  ): Into[C1[A], C2[B]] = new Into[C1[A], C2[B]] {
    def into(input: C1[A]): Either[SchemaError, C2[B]] = traverse(input, factory)(elementInto)
  }

  implicit def mapInto[K, V, K2, V2](implicit ki: Into[K, K2], vi: Into[V, V2], ks: Schema[K]): Into[Map[K, V], Map[K2, V2]] = new Into[Map[K, V], Map[K2, V2]] {
    def into(input: Map[K, V]): Either[SchemaError, Map[K2, V2]] = {
      val builder = Map.newBuilder[K2, V2]
      var errors: List[SchemaError.Single] = Nil
      val it = input.iterator
      while (it.hasNext) {
        val (k, v) = it.next()
        ki.into(k) match {
          case Left(e) =>
            errors = prependPath(e, DynamicOptic.Node.AtMapKey(ks.toDynamicValue(k))) ::: errors
          case Right(k2) =>
            vi.into(v) match {
              case Left(e) =>
                errors = prependPath(e, DynamicOptic.Node.AtMapKey(ks.toDynamicValue(k))) ::: errors
              case Right(v2) =>
                if (errors.isEmpty) builder += (k2 -> v2)
            }
        }
      }
      if (errors.isEmpty) Right(builder.result())
      else Left(SchemaError(scala.collection.immutable.List.from(errors.reverse).asInstanceOf[::[SchemaError.Single]]))
    }
  }

  implicit def eitherInto[L1, R1, L2, R2](implicit l: Into[L1, L2], r: Into[R1, R2]): Into[Either[L1, R1], Either[L2, R2]] = new Into[Either[L1, R1], Either[L2, R2]] {
    def into(input: Either[L1, R1]): Either[SchemaError, Either[L2, R2]] = input match {
      case Left(v) => l.into(v).map(Left(_))
      case Right(v) => r.into(v).map(Right(_))
    }
  }

  private[schema] def traverse[A, B, In <: Iterable[A], Out](
      collection: In,
      factory: Factory[B, Out]
  )(implicit into: Into[A, B]): Either[SchemaError, Out] = {
    val builder = factory.newBuilder
    var errors: List[SchemaError.Single] = Nil
    var index = 0
    val it = collection.iterator
    while (it.hasNext) {
      val a = it.next()
      into.into(a) match {
        case Right(b) => if (errors.isEmpty) builder += b
        case Left(e) =>
          errors = prependPath(e, DynamicOptic.Node.AtIndex(index)) ::: errors
      }
      index += 1
    }
    if (errors.isEmpty) Right(builder.result())
    else Left(SchemaError(scala.collection.immutable.List.from(errors.reverse).asInstanceOf[::[SchemaError.Single]]))
  }

  private[schema] def prependPath(e: SchemaError, node: DynamicOptic.Node): List[SchemaError.Single] = {
    e.errors.map { single =>
      val newSource = DynamicOptic(Vector(node) ++ single.source.nodes)
      single match {
        case x: SchemaError.MissingField => x.copy(source = newSource)
        case x: SchemaError.DuplicatedField => x.copy(source = newSource)
        case x: SchemaError.ExpectationMismatch => x.copy(source = newSource)
        case x: SchemaError.UnknownCase => x.copy(source = newSource)
        case x: SchemaError.ValidationFailed => x.copy(source = newSource)
      }
    }
  }
}

trait IntoLowPriority {
  implicit def fromAsReverse[A, B](implicit as: As[A, B]): Into[B, A] = new Into[B, A] {
    def into(input: B): Either[SchemaError, A] = as.from(input)
  }
}
