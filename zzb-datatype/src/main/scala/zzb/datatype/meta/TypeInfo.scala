package zzb.datatype.meta

import spray.json._
import zzb.datatype.StructRegistry

/**
 * Created by Simon on 2014/4/17
 */


case class FieldDesc(code: String, typeInfo: TypeInfo, require: Boolean, withDefault: Boolean)

object FieldDesc extends DefaultJsonProtocol {
  implicit val format = new RootJsonFormat[FieldDesc] {
    override def write(c: FieldDesc): JsValue = {
      val fields = Map(
        "code" -> JsString(c.code),
        "type" -> JsString(c.typeInfo.valueCode),
        "require" -> JsBoolean(c.require),
        "withDefault" -> JsBoolean(c.withDefault),
        "memo" -> JsString(c.typeInfo.memo)
      )
      val finalFields =  c.typeInfo match {
        case et : EnumTypeInfo => fields + ("enum" -> JsArray(et.values.map(f=>JsObject(Map("name" ->JsString(f._2),"value" -> JsString(f._1.toString)))):_*))
        case _ => fields
      }
      JsObject(finalFields)
    }

    override def read(value: JsValue) = value match {
      case JsObject(fields) =>
        new FieldDesc(
          fields("code") match {
            case JsString(code) => code
            case _ => deserializationError("FieldDesc expected")
          },
          (fields("type"), fields("memo")) match {
            case (JsString(typeName), JsString(memo)) => StructRegistry.get(typeName) match {
              case Some(structType) => structType.typeInfo
              case None => new TypeInfo(typeName, memo)
            }
            case _ => deserializationError("FieldDesc expected")
          },
          fields("require") match {
            case JsBoolean(require) => require
            case _ => deserializationError("FieldDesc expected")
          },
          fields("withDefault") match {
            case JsBoolean(withDefault) => withDefault
            case _ => deserializationError("FieldDesc expected")
          }
        )
      case _ => deserializationError("FieldDesc expected")

    }
  }
}

class TypeInfo(val valueCode: String, val memo: String, val fields: List[FieldDesc] = Nil){
  require(!valueCode.isEmpty)
   val simpleCode = valueCode.split('.').toList.reverse.head

  def hasChild = fields.size > 0

  override def toString = simpleCode
}
class ListTypeInfo(valueCode:String, memo :String) extends TypeInfo(valueCode,memo,Nil){
  override def toString = s"List[$simpleCode]"

  override def hasChild = true
}

class MapTypeInfo(val keyCode :String,valueCode:String, memo :String) extends TypeInfo(valueCode,memo,Nil){
  lazy val simpleKeyCode = keyCode.split('.').toList.reverse.head

  override def toString = s"Map[$simpleKeyCode,$simpleCode]"

  override def hasChild = true
}

object TypeInfo extends DefaultJsonProtocol {
  def shortName(fullName:String):String = {
    fullName.lastIndexOf(".") match {
      case -1 => fullName
      case idx => fullName.substring(idx+1)
    }
  }
  implicit val format = new RootJsonFormat[TypeInfo] {

    override def write(c: TypeInfo): JsValue = {
      var m = Map[String, JsValue](
        "typeid" -> JsString(c.valueCode),
        "memo" -> JsString(c.memo)
      )
      if (c.fields.size > 0){
        val fieldMap  = c.fields.map(field => shortName(field.typeInfo.valueCode) -> FieldDesc.format.write(field)).toMap
        m = m + ("fields" -> JsObject(fieldMap))
      }

      JsObject(m)
    }

    override def read(json: JsValue): TypeInfo = json match {
      case JsObject(jsFields) =>
        new TypeInfo(
          jsFields("typeid") match {
            case JsString(name) => name
            case _ => deserializationError("TypeInfo expected")
          },
          jsFields("memo") match {
            case JsString(memo) => memo
            case _ => deserializationError("TypeInfo expected")
          },
          if (jsFields.contains("fields"))
            jsFields("fields") match {
              case JsObject(fields) => fields.values.toList.map(FieldDesc.format.read)
              case _ => Nil
            }
          else Nil
        )
      case _ => deserializationError("TypeInfo expected")
    }
  }
}

class EnumTypeInfo(override val valueCode: String,override val memo: String,val values:List[(Int,String)]) extends TypeInfo(valueCode,memo,Nil){
  override def hasChild = true

  override def toString = s"Enum[$simpleCode]"
}
object EnumTypeInfo extends DefaultJsonProtocol {
  implicit val format = new RootJsonFormat[EnumTypeInfo] {
    override def write(c: EnumTypeInfo): JsValue = {
      var m = Map[String, JsValue](
        "typeid" -> JsString(c.valueCode),
        "memo" -> JsString(c.memo)
      )
      if (c.values.size > 0)
        m = m + ("values" -> JsObject(c.values.map(f=>f._1.toString ->JsString(f._2)):_*))
      JsObject(m)
    }

    override def read(json: JsValue): EnumTypeInfo = json match {
      case JsObject(jsFields) =>
        new EnumTypeInfo(
          jsFields("typeid") match {
            case JsString(name) => name
            case _ => deserializationError("TypeInfo expected")
          },
          jsFields("memo") match {
            case JsString(memo) => memo
            case _ => deserializationError("TypeInfo expected")
          },
          if (jsFields.contains("values"))
            jsFields("values") match {
              case JsObject(fields) => fields.map(f=>f._1.toInt -> f._2.toString()).toList
              case _ => Nil
            }
          else Nil
        )
      case _ => deserializationError("EnumTypeInfo expected")
    }
  }
}
