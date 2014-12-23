package zzb

import java.io.{StringWriter, PrintWriter}

/**
 * Created by Simon on 2014/6/30
 */
package object util {

  implicit class ExceptionStackTrace(ex :Throwable){
    def stackTrace = {
      val errors = new StringWriter()
      ex.printStackTrace(new PrintWriter(errors))
      errors.toString
    }
  }
}
