package zio.blocks.schema

import zio.test._
import zio.test.Assertion._
import zio.blocks.schema.SchemaError._

object IntoSpec extends ZIOSpecDefault {

  case class A1(x: Int, y: String)
  case class B1(x: Int, y: String)

  case class A2(x: Int, y: String, z: Boolean)
  case class B2(x: Int, y: String)

  case class A3(x: Int, y: String)
  case class B3(y: String, x: Int)

  case class A4(x: Int)
  case class B4(x: Long)

  case class A5(x: Int)
  case class B5(x: Int, y: String = "default")

  case class A6(x: Int)
  case class B6(x: Int, y: Option[String])

  object Source {
    sealed trait ST
    case object CaseA extends ST
    case class CaseB(i: Int) extends ST
  }

  object Target {
    sealed trait ST
    case object CaseA extends ST
    case class CaseB(i: Int) extends ST
  }

  def spec = suite("IntoSpec")(
    suite("Derivation")(
      test("case class same fields") {
         val into = Into[A1, B1]
         assert(into.into(A1(1, "s")))(isRight(equalTo(B1(1, "s"))))
      },
      test("case class extra field in A") {
         val into = Into[A2, B2]
         assert(into.into(A2(1, "s", true)))(isRight(equalTo(B2(1, "s"))))
      },
      test("case class field reordering") {
         val into = Into[A3, B3]
         assert(into.into(A3(1, "s")))(isRight(equalTo(B3("s", 1))))
      },
      test("case class coercion (Int -> Long)") {
         val into = Into[A4, B4]
         assert(into.into(A4(1)))(isRight(equalTo(B4(1L))))
      },
      test("case class default value") {
         val into = Into[A5, B5]
         assert(into.into(A5(1)))(isRight(equalTo(B5(1, "default"))))
      },
      test("case class option default") {
         val into = Into[A6, B6]
         assert(into.into(A6(1)))(isRight(equalTo(B6(1, None))))
      },
      test("sealed trait mapping") {
         val into = Into[Source.ST, Target.ST]
         assert(into.into(Source.CaseA))(isRight(equalTo(Target.CaseA: Target.ST))) &&
         assert(into.into(Source.CaseB(1)))(isRight(equalTo(Target.CaseB(1): Target.ST)))
      },
      test("tuple to case class (positional)") {
         val into = Into[(Int, String), A1]
         assert(into.into((1, "s")))(isRight(equalTo(A1(1, "s"))))
      }
    ),
    suite("Collection")(
       test("List -> List") {
          val into = implicitly[Into[List[Int], List[Long]]]
          assert(into.into(List(1, 2)))(isRight(equalTo(List(1L, 2L))))
       },
       test("List -> Vector") {
          val into = implicitly[Into[List[Int], Vector[Long]]]
          assert(into.into(List(1, 2)))(isRight(equalTo(Vector(1L, 2L))))
       },
       test("Map -> Map") {
          val into = implicitly[Into[Map[Int, String], Map[Long, String]]]
          assert(into.into(Map(1 -> "s")))(isRight(equalTo(Map(1L -> "s"))))
       }
    )
  )
}
