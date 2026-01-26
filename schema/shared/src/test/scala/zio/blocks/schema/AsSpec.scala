package zio.blocks.schema

import zio.test._
import zio.test.Assertion._

object AsSpec extends ZIOSpecDefault {

  case class A1(x: Int)
  case class B1(x: Int)

  def spec = suite("AsSpec")(
     test("round trip case class") {
        val as = As[A1, B1]
        assert(as.into(A1(1)))(isRight(equalTo(B1(1)))) &&
        assert(as.from(B1(1)))(isRight(equalTo(A1(1))))
     },
     test("primitive narrowing failure") {
        val as = As[Int, Long]
        assert(as.into(1))(isRight(equalTo(1L))) &&
        assert(as.from(1L))(isRight(equalTo(1))) &&
        assert(as.from(Long.MaxValue))(isLeft(anything))
     },
     test("List round trip") {
        val as = As[List[Int], List[Long]]
        assert(as.into(List(1, 2)))(isRight(equalTo(List(1L, 2L)))) &&
        assert(as.from(List(1L, 2L)))(isRight(equalTo(List(1, 2))))
     }
  )
}
