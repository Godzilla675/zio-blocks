import zio.http._
import zio._

import scala.annotation.nowarn

object Reproducer extends ZIOAppDefault {
  case class WebSession(id: Int)

  // The bug was caused by Handler.fromFunctionZIO[Request] which forces the input to be cast to Request.
  // When used on a route with path parameters, the input is a Tuple (Params, Request).
  // This incorrect cast caused a ClassCastException downstream or earlier.
  // The fix is to accept Any input, and pattern match to extract the Request while preserving the input structure.
  def maybeWebSession: HandlerAspect[Any, Option[WebSession]] =
    HandlerAspect.interceptIncomingHandler(
      Handler.fromFunctionZIO[Any] {
        case req: Request =>
          // Case: Route has no path parameters, input is just Request.
          ZIO.succeed((req, None))
        case (input, req: Request) =>
          // Case: Route has path parameters. Input is (Params, Request).
          // We must preserve the full input tuple so the downstream handler
          // (which expects (Params, Request)) receives it correctly.
          // We also attach the context (None) as the second element.
          ZIO.succeed(((input, req), None))
        case input =>
          ZIO.dieMessage(s"Middleware expected Request or (Any, Request) but got: ${input.getClass.getName}")
      }
    )

  @nowarn("msg=dead code following this construct")
  override def run: ZIO[Any, Any, Any] = {
    // fails at Handler.scala line 57 with
    val route = Method.GET / "base" / string("1") -> handler((a: String, req: Request) => {
      withContext((c: Option[WebSession]) => {
        ZIO.logInfo("Hello").as(Response.ok)
      })
    }) @@ maybeWebSession



    (for {
      port <- Server.install(Routes(route))
      _ <- ZIO.logInfo(s"Installed on port " + port)
      _ <- ZIO.never
    } yield ()).provide(Server.default)

  }
}
