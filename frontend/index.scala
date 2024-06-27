//> using platform scala-js
//> using dep com.raquo::laminar::17.0.0
//> using dep io.laminext::websocket::0.17.0
//> using scala 3.5.0-RC2

import io.laminext.websocket._

import com.raquo.laminar.api.L.*
import scala.concurrent.Future
import org.scalajs.dom.Fetch
import org.scalajs.dom.RequestInit
import org.scalajs.dom.HttpMethod
import scala.concurrent.ExecutionContext.Implicits.global
import io.laminext.websocket.WebSocketEvent.Received
import scala.scalajs.js.JSON

val N = 1_000
val ROW_SIZE = 100

case class Coordinates(row: Int /* row */, col: Int /* column */ )

trait Api:
  def toggle(id: Int): Future[Unit]

case class FetchApi(base: String) extends Api:
  override def toggle(id: Int): Future[Unit] =
    val ri = new RequestInit {}
    ri.method = HttpMethod.POST
    val url = s"$base/click/$id"
    Fetch.fetch(url, ri).toFuture.map(response => assert(response.ok))

def app(state: Var[Array[Boolean]], api: Api, ws: WebSocket[String, String]) =

  val updater = state.updater[(Coordinates, Boolean)]:
    case (currentState, (Coordinates(row, col), value)) =>
      val location = row * ROW_SIZE + col
      currentState(location) = value
      currentState

  val updaterById = state.updater[(Int, Boolean)]:
    case (currentState, (id, value)) =>
      currentState(id) = value
      currentState

  val N_ROWS = N / ROW_SIZE

  div(
    ws.connect,
    for row <- 0 to N_ROWS
    yield
      val start = row * ROW_SIZE
      val endOffset = (start + ROW_SIZE).min(N - 1)
      div(
        for col <- 0 to endOffset
        yield
          val location = (row * ROW_SIZE + col).min(N - 1)
          input(
            tpe := "checkbox",
            checked <-- state.signal.map(_(location)),
            onClick --> { _ =>
              api.toggle(location)
            },
            alt := s"$row - $col"
          )
      )
    ,
    ws.received
      .map: str =>
        val json = JSON.parse(str)
        val id = json.id.toString.toInt
        val value = json.value.toString.toBoolean
        (id -> value)
      .-->(updaterById)
  )
end app

@main def hello =
  val state = Var(Array.fill(N)(false))
  val api = FetchApi("")

  val ws = WebSocket.path("/ws").string.build()

  renderOnDomContentLoaded(
    org.scalajs.dom.document.getElementById("content"),
    app(state, api, ws)
  )

end hello
