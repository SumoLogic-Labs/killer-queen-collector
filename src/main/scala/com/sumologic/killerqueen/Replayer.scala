package com.sumologic.killerqueen

import java.text.SimpleDateFormat
import java.util.Date

import com.sumologic.killerqueen.events.{EventHandler, EventParser, NoopEventSender}

/**
  * Replays included log files.  Very useful for getting a known data set to iterate your changes on.  At the end, it
  * prints a time stamp that is compatible with searching in Sumo Logic.
  */
object Replayer extends App with Logging {

  def replayFile(file: String): Unit = {
    // Based off https://stackoverflow.com/a/51686476
    val fileStream = getClass.getResourceAsStream(file)
    val lines = scala.io.Source.fromInputStream(fileStream).getLines

    val eventHandler = new EventHandler(NoopEventSender)

    val startTime = System.currentTimeMillis()

    lines.foreach {
      line =>
        val parsedEvent = EventParser.parse(line)
        eventHandler.handle(parsedEvent)
        Thread.sleep(1) // We need to ensure that game IDs are unique and that gameplay events come in the correct order
    }

    val timeTaken = System.currentTimeMillis() - startTime

    info(s"Replay of $file took $timeTaken ms.")
  }

  val startDate = new Date()
  startDate.setTime(startDate.getTime - 1000)

  replayFile("/raw_event_log_2018-09-18.txt")
  replayFile("/raw_event_log_2018-09-19.txt")
  replayFile("/raw_event_log_2018-09-20.txt")
  replayFile("/raw_event_log_2018-09-21.txt")
  replayFile("/raw_event_log_2018-09-26.txt")
  replayFile("/raw_event_log_2018-09-28.txt")
  replayFile("/raw_event_log_2018-10-01.txt")
  replayFile("/raw_event_log_2018-10-02.txt")
  replayFile("/raw_event_log_2018-10-03.txt")
  replayFile("/raw_event_log_2018-10-04.txt")
  replayFile("/raw_event_log_2018-10-05.txt")
  replayFile("/raw_event_log_2018-10-22.txt")

  val endDate = new Date()
  endDate.setTime(endDate.getTime + 1000)

  // 09/19/2018 15:51:57 09/19/2018 15:52:20  <-- end goal format
  val formatter = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss")
  info(s"Logs are searchable with ${formatter.format(startDate)} ${formatter.format(endDate)}")


  info("Sleeping for 15s to make sure all events flush.")
  Thread.sleep(15 * 1000) // Sleeping to make sure sumo buffers flushes

}
