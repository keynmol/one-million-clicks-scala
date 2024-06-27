//> using dep org.http4s::http4s-ember-server::0.23.27
//> using dep org.http4s::http4s-dsl::0.23.27
//> using scala 3.5.0-RC2

import org.http4s.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.{WebSocketFrame => Frame}

import cats.effect.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import scala.concurrent.duration.*
import cats.syntax.all.*
import fs2.concurrent.Topic

trait Store:
  def toggle(id: Int): IO[Boolean]

private class RefStore(size: Int, ref: Ref[IO, Vector[Boolean]]) extends Store {

  override def toggle(id: Int): IO[Boolean] =
    ref
      .modify: curState =>
        val currentValue = curState(id)
        (curState.updated(id, !currentValue), !currentValue)

}

object Store:
  def inMemory(size: Int): IO[Store] =
    IO.ref[Vector[Boolean]](Vector.fill(size)(false))
      .map: ref =>
        new RefStore(size, ref)

def routes(
    builder: WebSocketBuilder2[IO],
    store: Store,
    size: Int,
    topic: Topic[IO, (Int, Boolean)]
) =
  HttpRoutes.of[IO] {

    case GET -> Root / "ws" =>
      val toClient: fs2.Stream[IO, Frame] =
        topic
          .subscribe(1024)
          .map((id, value) => Frame.Text(s"{$"id$": $id, $"value$": $value}"))
          .evalTap(frame => IO.println(s"Sending $frame"))

      val fromClient: fs2.Pipe[IO, Frame, Unit] = _.void

      builder.build(toClient, fromClient)

    case POST -> Root / "click" / buttonId =>
      buttonId.toIntOption match
        case None =>
          BadRequest("oh no no no no no no bobob")
        case Some(id) if id < 0 || id >= size =>
          BadRequest(s"Number is out of bounds ($size)")
        case Some(id) =>
          store.toggle(id).flatMap(value => topic.publish1(id -> value)) *>
            Ok("Much success")

  }

val SIZE = 1000

object OneThousandClicks extends IOApp.Simple:
  override def run: IO[Unit] =
    Topic[IO, (Int, Boolean)].toResource
      .flatMap: topic =>
        Store
          .inMemory(SIZE)
          .toResource
          .flatMap: store =>
            EmberServerBuilder
              .default[IO]
              .withHttpWebSocketApp(builder =>
                routes(builder, store, SIZE, topic).orNotFound
              )
              .withShutdownTimeout(100.millis)
              .build
              .evalTap(server => IO.println(server.baseUri))
      .useForever
      .void
