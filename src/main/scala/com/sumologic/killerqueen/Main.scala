package com.sumologic.killerqueen

import java.io.{File => JFile}
import java.util.{Timer, TimerTask}

import akka.Done
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest, WebSocketUpgradeResponse}
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.directives.ContentTypeResolver.Default
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.sumologic.killerqueen.events.{ActorRefEventSender, EventHandler, EventParser, RawMessageRecorder}
import com.sumologic.killerqueen.model.InboundEvents.UserNameUpdateEvent
import com.sumologic.killerqueen.state.StateMachine
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future

object Main extends App with Logging {
  private implicit val system = ActorSystem()
  private implicit val materializer = ActorMaterializer()
  private implicit val executionContext = system.dispatcher

  private implicit val userNameFormat = jsonFormat10(UserNameUpdateEvent)

  private val logFile = new JFile("./logs/raw.log")
  private val messageRecorder = new RawMessageRecorder(logFile)

  private val stateMachine: StateMachine = new StateMachine
  startWebserver(stateMachine)
  connectToCabinet(stateMachine)
  startLoggingClock(stateMachine)

  private def startWebserver(stateMachine: StateMachine): Unit = {
    val route =
      path("hello") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
        }
      } ~ path("index.html") {
        getFromResource("index.html")
      } ~ path("jquery-3.4.1.min.js") {
        getFromResource("jquery-3.4.1.min.js")
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
      } ~ pathPrefix("names") {
        lazy val currentUser = stateMachine.currentUsers

        def safeGet(i: Int): String = currentUser.get(i).getOrElse("Botholomew")

        def safeRender(team: String, offset: Int): StandardRoute = {
          val autoRefresh = s"<script>setTimeout(function() {location.href = location.href;}, 5000);</script>"
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
            s"<h1>${safeGet(3 + offset)} - ${safeGet(5 + offset)} - ${safeGet(1 + offset)} - ${safeGet(7 + offset)} - ${safeGet(9 + offset)}</h1> " + autoRefresh))
        }

        path("blue") {
          safeRender("blue", offset = 1)
        } ~ path("gold") {
          safeRender("gold", offset = 0)
        }
      }

    Http().bindAndHandle(route, "localhost", 8080)

    info(s"Server online at http://localhost:8080/ - Logging to ${logFile.getPath}")
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
          messageRecorder.writeToFile(message.text)
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

  private def startLoggingClock(machine: StateMachine): Unit = {
    val task = new TimerTask {
      override def run(): Unit = machine.logCurrentGameState()
    }

    new Timer(true).schedule(task, 0, 1000)
  }
}
