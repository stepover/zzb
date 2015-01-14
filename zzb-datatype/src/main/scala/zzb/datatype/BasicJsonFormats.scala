package zzb.datatype

import spray.json._

/**
 * Created by Simon on 2015/1/14 
 */
object BasicJsonFormats {

  val TStringJsonFormat = BasicFormats.StringJsonFormat

  implicit object TIntJsonFormat extends JsonFormat[Int] {
    def write(x: Int) = JsNumber(x)

    def read(value: JsValue) = value match {
      case JsNumber(x) => x.intValue()
      case JsString(x) => try {
        Integer.parseInt(x)
      } catch {
        case e: Throwable => deserializationError("Expected Int as JsNumber, but got " + x)
      }
      case x => deserializationError("Expected Int as JsNumber, but got " + x)
    }
  }

  implicit object TLongJsonFormat extends JsonFormat[Long] {
    def write(x: Long) = JsNumber(x)
    def read(value: JsValue) = value match {
      case JsNumber(x) => x.longValue()
      case JsString(x) => try {
        java.lang.Long.parseLong(x)
      } catch {
        case e: Throwable => deserializationError("Expected Long as JsNumber, but got " + x)
      }
      case x => deserializationError("Expected Long as JsNumber, but got " + x)
    }
  }

  implicit object TFloatJsonFormat extends JsonFormat[Float] {
    def write(x: Float) = JsNumber(x)
    def read(value: JsValue) = value match {
      case JsNumber(x) => x.floatValue()
      case JsString(x) => try {
        java.lang.Float.parseFloat(x)
      } catch {
        case e: Throwable => deserializationError("Expected Float as JsNumber, but got " + x)
      }
      case JsNull      => Float.NaN
      case x => deserializationError("Expected Float as JsNumber, but got " + x)
    }
  }

  implicit object TDoubleJsonFormat extends JsonFormat[Double] {
    def write(x: Double) = JsNumber(x)
    def read(value: JsValue) = value match {
      case JsNumber(x) => x.doubleValue()
      case JsString(x) => try {
        java.lang.Double.parseDouble(x)
      } catch {
        case e: Throwable => deserializationError("Expected Double as JsNumber, but got " + x)
      }
      case JsNull      => Double.NaN
      case x => deserializationError("Expected Double as JsNumber, but got " + x)
    }
  }

  implicit object TByteJsonFormat extends JsonFormat[Byte] {
    def write(x: Byte) = JsNumber(x)
    def read(value: JsValue) = value match {
      case JsNumber(x) => x.byteValue()
      case JsString(x) => try {
        java.lang.Byte.parseByte(x)
      } catch {
        case e: Throwable => deserializationError("Expected Byte as JsNumber, but got " + x)
      }
      case x => deserializationError("Expected Byte as JsNumber, but got " + x)
    }
  }

  implicit object TShortJsonFormat extends JsonFormat[Short] {
    def write(x: Short) = JsNumber(x)
    def read(value: JsValue) = value match {
      case JsNumber(x) => x.shortValue()
      case JsString(x) => try {
        java.lang.Short.parseShort(x)
      } catch {
        case e: Throwable => deserializationError("Expected Byte as JsNumber, but got " + x)
      }
      case x => deserializationError("Expected Short as JsNumber, but got " + x)
    }
  }

  implicit object TBigDecimalJsonFormat extends JsonFormat[BigDecimal] {
    def write(x: BigDecimal) = {
      require(x ne null)
      JsNumber(x)
    }
    def read(value: JsValue) = value match {
      case JsNumber(x) => x
      case JsString(x) => try {
        BigDecimal.apply(x)
      } catch {
        case e: Throwable => deserializationError("Expected BigDecimal as JsNumber, but got " + x)
      }
      case x => deserializationError("Expected BigDecimal as JsNumber, but got " + x)
    }
  }

  implicit object TBooleanJsonFormat extends JsonFormat[Boolean] {
    def write(x: Boolean) = JsBoolean(x)
    def read(value: JsValue) = value match {
      case JsTrue => true
      case JsFalse => false
      case JsString(x) => try {
        TBoolean.parse(x).value
      } catch {
        case e: Throwable => deserializationError("Expected JsBoolean, but got " + x)
      }
      case x => deserializationError("Expected JsBoolean, but got " + x)
    }
  }


}


object BasicFormats extends BasicFormats