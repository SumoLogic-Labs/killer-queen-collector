package com.sumologic.killerqueen

import akka.Done
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest, WebSocketUpgradeResponse}
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.ContentTypeResolver.Default
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.sumologic.killerqueen.events.{ActorRefEventSender, EventHandler, EventParser}
import com.sumologic.killerqueen.model.InboundEvents.UserNameUpdateEvent
import com.sumologic.killerqueen.state.StateMachine
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future

object Main extends App with Logging {
  private implicit val system = ActorSystem()
  private implicit val materializer = ActorMaterializer()
  private implicit val executionContext = system.dispatcher

  private implicit val userNameFormat = jsonFormat10(UserNameUpdateEvent)

  private val stateMachine: StateMachine = new StateMachine
  startWebserver(stateMachine)
  connectToCabinet(stateMachine)

  private def startWebserver(stateMachine: StateMachine): Unit = {
    val route =
      path("hello") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
        }
      } ~ path("index.html") {
        getFromResource("index.html")
      } ~ path("jquery-3.3.1.min.js") {
        getFromResource("jquery-3.3.1.min.js")
      } ~ path("updateUserNames") {
        post {
          formFields('blue_stripes, 'blue_abs, 'blue_queen, 'blue_skulls, 'blue_checkers, 'gold_stripes, 'gold_abs, 'gold_queen, 'gold_skulls, 'gold_checkers) {
            (blueStripes, blueAbs, blueQueen, blueSkulls, blueCheckers, goldStripes, goldAbs, goldQueen, goldSkulls, goldCheckers) =>
              val update = UserNameUpdateEvent(blueStripes, blueAbs, blueQueen, blueSkulls, blueCheckers, goldStripes, goldAbs, goldQueen, goldSkulls, goldCheckers)
              debug(s"Received message ${TextMessage.Strict(update.toApi)}")
              stateMachine.processEvent(update)
              complete("done")
          }
        }
      }

    Http().bindAndHandle(route, "localhost", 8080)

    info(s"Server online at http://localhost:8080/")
  }

  private def connectToCabinet(stateMachine: StateMachine): Unit = {

    val req = WebSocketRequest(uri = "ws://kq.local:12749")
    val webSocketFlow = Http().webSocketClientFlow(req)

    val messageSource: Source[Message, ActorRef] =
      Source.actorRef[TextMessage.Strict](bufferSize = 10, OverflowStrategy.fail)

    var handlerOpt: Option[EventHandler] = None

    val messageSink: Sink[Message, Future[Done]] =
      Sink.foreach[Message] {
        case message: TextMessage.Strict =>
          handlerOpt match {
            case Some(handler) =>
              debug(s"Received message $message")
              val parsedEvent = EventParser.parse(message.text)
              handler.handle(parsedEvent)
            case _ =>
              warn(s"Received message $message but unable to process it - handler is not ready")
          }
      }

    val ((ws: ActorRef, upgradeResponse: Future[WebSocketUpgradeResponse]), closed: Future[Done]) =
      messageSource
        .viaMat(webSocketFlow)(Keep.both)
        .toMat(messageSink)(Keep.both)
        .run()

    handlerOpt = Some(new EventHandler(new ActorRefEventSender(ws), stateMachine))

    info("Connecting to KQ cabinet")

    upgradeResponse.flatMap { upgrade =>
      if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
        info("Connected to KQ cabinet")
        Future.successful(Done)
      } else {
        error(s"Connection failed: ${upgrade.response.status}")
        throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
      }
    }

    closed.onComplete {
      _ =>
        info("Closed connection to cabinet.  Retrying connection")
        connectToCabinet(stateMachine)
    }

    info("Attempt to start connection to cabinet done.  Waiting.")

    ws ! TextMessage.Strict("![k[connect],v[{\"name\":\"1\",\"isGameMachine\":false}]]!")
  }
}