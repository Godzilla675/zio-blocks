package zio.blocks.schema

import scala.collection.Factory

trait As[A, B] extends Into[A, B] {
  def into(input: A): Either[SchemaError, B]
  def from(input: B): Either[SchemaError, A]
}

object As extends AsCompanionVersionSpecific {
  def apply[A, B](implicit as: As[A, B]): As[A, B] = as

  implicit def identity[A]: As[A, A] = new As[A, A] {
    def into(input: A): Either[SchemaError, A] = Right(input)
    def from(input: A): Either[SchemaError, A] = Right(input)
  }

  implicit val byteToShort: As[Byte, Short] = new As[Byte, Short] {
    def into(a: Byte): Either[SchemaError, Short] = Right(a.toShort)
    def from(b: Short): Either[SchemaError, Byte] =
      if (b >= Byte.MinValue && b <= Byte.MaxValue) Right(b.toByte)
      else Left(SchemaError.validationFailed(Nil, s"Short $b out of Byte range"))
  }

  implicit val shortToInt: As[Short, Int] = new As[Short, Int] {
    def into(s: Short): Either[SchemaError, Int] = Right(s.toInt)
    def from(i: Int): Either[SchemaError, Short] =
      if (i >= Short.MinValue && i <= Short.MaxValue) Right(i.toShort)
      else Left(SchemaError.validationFailed(Nil, s"Int $i out of Short range"))
  }

  implicit val intToLong: As[Int, Long] = new As[Int, Long] {
    def into(i: Int): Either[SchemaError, Long] = Right(i.toLong)
    def from(l: Long): Either[SchemaError, Int] =
      if (l >= Int.MinValue && l <= Int.MaxValue) Right(l.toInt)
      else Left(SchemaError.validationFailed(Nil, s"Long $l out of Int range"))
  }

  implicit val floatToDouble: As[Float, Double] = new As[Float, Double] {
    def into(f: Float): Either[SchemaError, Double] = Right(f.toDouble)
    def from(d: Double): Either[SchemaError, Float] = {
       // Check for overflow/underflow if strictly required, but standard narrowing cast is:
       val f = d.toFloat
       if (f.isInfinite && !d.isInfinite) Left(SchemaError.validationFailed(Nil, s"Double $d overflows Float"))
       else Right(f)
    }
  }

  implicit val intToDouble: As[Int, Double] = new As[Int, Double] {
    def into(i: Int): Either[SchemaError, Double] = Right(i.toDouble)
    def from(d: Double): Either[SchemaError, Int] =
      if (d.isValidInt) Right(d.toInt)
      else Left(SchemaError.validationFailed(Nil, s"Double $d is not a valid Int"))
  }

  implicit def optionAs[A, B](implicit as: As[A, B]): As[Option[A], Option[B]] = new As[Option[A], Option[B]] {
    def into(input: Option[A]): Either[SchemaError, Option[B]] = input match {
      case Some(a) => as.into(a).map(Some(_))
      case None => Right(None)
    }
    def from(input: Option[B]): Either[SchemaError, Option[A]] = input match {
      case Some(b) => as.from(b).map(Some(_))
      case None => Right(None)
    }
  }

  implicit def listAs[A, B](implicit as: As[A, B]): As[List[A], List[B]] = new As[List[A], List[B]] {
    def into(input: List[A]): Either[SchemaError, List[B]] =
      Into.traverse(input, implicitly[Factory[B, List[B]]])(new Into[A, B] { def into(a: A) = as.into(a) })
    def from(input: List[B]): Either[SchemaError, List[A]] =
      Into.traverse(input, implicitly[Factory[A, List[A]]])(new Into[B, A] { def into(b: B) = as.from(b) })
  }

  implicit def vectorAs[A, B](implicit as: As[A, B]): As[Vector[A], Vector[B]] = new As[Vector[A], Vector[B]] {
    def into(input: Vector[A]): Either[SchemaError, Vector[B]] =
      Into.traverse(input, implicitly[Factory[B, Vector[B]]])(new Into[A, B] { def into(a: A) = as.into(a) })
    def from(input: Vector[B]): Either[SchemaError, Vector[A]] =
      Into.traverse(input, implicitly[Factory[A, Vector[A]]])(new Into[B, A] { def into(b: B) = as.from(b) })
  }

  implicit def mapAs[K, V, K2, V2](implicit ki: As[K, K2], vi: As[V, V2], ks: Schema[K], k2s: Schema[K2]): As[Map[K, V], Map[K2, V2]] = new As[Map[K, V], Map[K2, V2]] {
    def into(input: Map[K, V]): Either[SchemaError, Map[K2, V2]] =
      Into.mapInto(new Into[K, K2] { def into(k: K) = ki.into(k) }, new Into[V, V2] { def into(v: V) = vi.into(v) }, ks).into(input)

    def from(input: Map[K2, V2]): Either[SchemaError, Map[K, V]] =
      Into.mapInto(new Into[K2, K] { def into(k: K2) = ki.from(k) }, new Into[V2, V] { def into(v: V2) = vi.from(v) }, k2s).into(input)
  }

  implicit def eitherAs[L1, R1, L2, R2](implicit l: As[L1, L2], r: As[R1, R2]): As[Either[L1, R1], Either[L2, R2]] = new As[Either[L1, R1], Either[L2, R2]] {
    def into(input: Either[L1, R1]): Either[SchemaError, Either[L2, R2]] = input match {
      case Left(v) => l.into(v).map(Left(_))
      case Right(v) => r.into(v).map(Right(_))
    }
    def from(input: Either[L2, R2]): Either[SchemaError, Either[L1, R1]] = input match {
      case Left(v) => l.from(v).map(Left(_))
      case Right(v) => r.from(v).map(Right(_))
    }
  }
}
