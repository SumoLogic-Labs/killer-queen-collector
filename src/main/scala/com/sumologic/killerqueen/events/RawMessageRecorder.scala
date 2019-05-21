package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.Logging

import scala.reflect.io.File
import scala.util.control.NonFatal

/**
  * Writes incoming messages to a file.
  *
  * @param file
  */
class RawMessageRecorder(file: File) extends Logging {

  file.createFile() // Ensure it exists

  def writeToFile(message: String): Unit = {
    try {
      synchronized {
        file.appendAll(message, "\n")
      }
    } catch {
      case NonFatal(e) =>
        warn(e, s"Unable to write $message to the file.")
    }
  }

}
