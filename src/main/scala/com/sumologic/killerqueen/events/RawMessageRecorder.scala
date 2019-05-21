package com.sumologic.killerqueen.events

import java.io.{File, FileWriter}

import com.sumologic.killerqueen.Logging

import scala.util.control.NonFatal

/**
  * Writes incoming messages to a file.
  *
  * @param file
  */
class RawMessageRecorder(file: File) extends Logging {

  file.createNewFile() // Ensure it exists

  def writeToFile(message: String): Unit = {
    try {
      val fw = new FileWriter(file, true)
      try {
        synchronized {
          fw.write(message)
          fw.write("\n")
        }
      } finally {
        fw.close()
      }
    } catch {
      case NonFatal(e) =>
        warn(e, s"Unable to write $message to the file.")
    }
  }

}
