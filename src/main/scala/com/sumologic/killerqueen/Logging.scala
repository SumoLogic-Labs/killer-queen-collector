package com.sumologic.killerqueen


import java.util.IllegalFormatException

import org.apache.logging.log4j.{LogManager, Logger}

/**
 * Convenience mix-in for easy logging
 */
trait Logging {

  @transient private var _logger = null.asInstanceOf[Logger]

  def logger: Logger = {
    if (_logger == null) {
      _logger = LogManager.getLogger(getClass.getName)
    }
    _logger
  }

  // Message only
  def debug(msg: => String, params: Any*): Unit = {
    if (logger.isDebugEnabled) {
      logger.debug(format(msg, params: _*))
    }
  }

  def error(msg: String, params: Any*): Unit = {
    if (logger.isErrorEnabled) {
      logger.error(format(msg, params: _*))
    }
  }

  def info(msg: String, params: Any*): Unit = {
    if (logger.isInfoEnabled) {
      logger.info(format(msg, params: _*))
    }
  }

  def warn(msg: String, params: Any*): Unit = {
    if (logger.isWarnEnabled) {
      logger.warn(format(msg, params: _*))
    }
  }

  // Throwable and message.

  def debug(ex: Throwable, msg: => String, params: Any*): Unit = {
    if (logger.isDebugEnabled) {
      logger.debug(format(msg, params: _*), ex)
    }
  }

  def error(ex: Throwable, msg: String, params: Any*): Unit = {
    if (logger.isErrorEnabled) {
      logger.error(format(msg, params: _*), ex)
    }
  }

  def info(ex: Throwable, msg: String, params: Any*): Unit = {
    if (logger.isInfoEnabled) {
      logger.info(format(msg, params: _*), ex)
    }
  }

  def warn(ex: Throwable, msg: String, params: Any*): Unit = {
    if (logger.isWarnEnabled) {
      logger.warn(format(msg, params: _*), ex)
    }
  }

  /**
   * Formats the message with any prefixes, etc.
   */
  private def format(msg: String, params: Any*): String = {
    if (params == null || params.length < 1) {
      msg
    } else {
      try {
        msg.format(params: _*)
      }
      catch {
        // Bail out if there's any illegal format exceptions and tell them the plain message.
        case e: IllegalFormatException =>
          msg + params.toString
      }
    }
  }
}
