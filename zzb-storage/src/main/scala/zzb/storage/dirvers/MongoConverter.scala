package zzb.storage.dirvers

import com.mongodb.{BasicDBObject, BasicDBList, DBObject}
import com.mongodb.casbah.commons.{MongoDBList, MongoDBObject}
import zzb.datatype._

import scala.collection.JavaConverters._

/**
 * zzb : zzb.storage.dirvers
 * Author: 飞飞狐
 * Date: 04/15 14:35
 */

object MongoConverter {

  import scala.collection.JavaConverters._

  def read(o: DBObject, dtp: DataType[_]): ValuePack[_] = {
    (o, dtp) match {
      case (dbo: BasicDBObject, d: TStruct) =>
        val fieldValues = o.keySet().asScala.filter(field => d.theFieldMap.contains(field)).map(k => k -> o.get(k)).map {
          case (k, v: DBObject) =>
            k -> read(v, d.theFieldMap(k))
          case (k, v) =>
            val aa = d.theFieldMap(k)
            val bb = d.theFieldMap(k).AnyToPack(v)
            k -> (d.theFieldMap(k) match {
              case datatype: TDateTime =>
                datatype.string2DatePack(v.asInstanceOf[String])
              case datatype: TInt => datatype.parse(v.toString)
              case datatype: TLong => datatype.parse(v.toString)
              case datatype: TShort => datatype.parse(v.toString)
              case datatype: TByte => datatype.parse(v.toString)
              case datatype: TDouble => datatype.parse(v.toString)
              case datatype: TFloat => datatype.parse(v.toString)
              case datatype: TBigDecimal => datatype.parse(v.toString)
              case datatype => datatype.AnyToPack(v).orNull
            })
        }.toMap
        d.makeValuePack(fieldValues)
      case (dbList: BasicDBList, d: TPackList[ValuePack[Any]]) =>
        val itemDt = d.itemDataType
        val iv = dbList.asScala.toList.map {
          case dbobj: DBObject =>
            read(dbobj, itemDt)
          case dbobj =>
            itemDt.AnyToPack(dbobj).orNull
        }
        d.value2Pack(iv)
      case (dbList: BasicDBList, d: TList[AnyRef]) =>
        val itemClass = d.lm.runtimeClass
        val iv = dbList.asScala.toList
        d.value2Pack(iv)
      case (dbo: BasicDBObject, d: TEnum) =>
        val idx = dbo.get("idx").asInstanceOf[Int]
        d.int2EnumPack(idx)
      case (dbo: BasicDBObject, d: TStrKeyPackMap[ValuePack[_]]) =>
        val itemDt = d.valueDataType
        val iv = dbo.keySet().asScala.toList.map { k =>
          k -> (dbo.get(k) match {
            case dbobj: DBObject =>
              read(dbobj, itemDt)
            case dbobj =>
              itemDt.AnyToPack(dbobj).orNull
          })
        }.toMap
        d.applyMapValue(iv)
      case (dbo: BasicDBObject, d: TMap[Any, Any]) =>
        val m = dbo.toMap.asScala.toSeq.toMap
        val r = d.applyMapValue(m)
        r
    }
  }

  def write(pack: ValuePack[_]): DBObject = {
    pack.dataType match {
      case dt: TStruct =>
        val sp = pack.asInstanceOf[TStruct#Pack]
        val vl = sp.value.fields.filter(_.value != null).map(v => write(v))
        val vm = vl.flatMap(dbo => dbo.toMap.asScala.toSeq).toMap
        MongoDBObject(sp.dataType.t_code_ -> vm)
      case dt: TDateTime =>
        val mp = pack.asInstanceOf[TDateTime#Pack]
        MongoDBObject(mp.dataType.t_code_ -> mp.value.toString("yyyy-MM-dd HH:mm:ss.SSS"))
      case dt: TBigDecimal =>
        val mp = pack.asInstanceOf[TBigDecimal#Pack]
        MongoDBObject(mp.dataType.t_code_ -> mp.value.toString())
      case dt: TEnum =>
        val mp = pack.asInstanceOf[TEnum#Pack]
        MongoDBObject(mp.dataType.t_code_ -> MongoDBObject("idx" -> mp.value.idx, "name" -> dt.int2Name(mp.value.idx)))
      case dt: TMono[_] =>
        val mp = pack.asInstanceOf[TMono[Any]#Pack]
        MongoDBObject(mp.dataType.t_code_ -> mp.value)
      case dt: TPackList[ValuePack[_]] =>
        val lp = pack.asInstanceOf[TPackList[ValuePack[_]]#Pack]
        val nd = lp.value.map(d => write(d)).flatMap(d => d.keySet().asScala.toList.map(d.get))
        MongoDBObject(lp.dataType.t_code_ -> MongoDBList(nd: _*))
      case dt: TList[_] =>
        val lp = pack.asInstanceOf[TList[Any]#Pack]
        MongoDBObject(lp.dataType.t_code_ -> MongoDBList(lp.value: _*))
      case dt: TStrKeyPackMap[ValuePack[_]] =>
        val mp = pack.asInstanceOf[TStrKeyPackMap[ValuePack[_]]#Pack]
        val nd = mp.value.map {
          case (k, v) => k -> write(v).get(v.dataType.t_code_)
        }
        MongoDBObject(pack.dataType.t_code_ -> nd)
      case dt: TMap[_, _] =>
        MongoDBObject(pack.dataType.t_code_ -> pack.value)
    }
  }
}