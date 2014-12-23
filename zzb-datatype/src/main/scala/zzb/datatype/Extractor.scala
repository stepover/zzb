package zzb.datatype

/**
 * Created by Simon on 2014/11/13 
 */

/**
 * 用于从数据类型值中按路径字符串提取对应的值串
 * @param content 被提取的内容
 * @param formatter 格式化器
 */
case class Extractor(content: Any, formatter: Extractor.Formatter = Extractor.defaultFormatter) {
  def apply(path: String) =
    content match {
      case pack: TStruct#Pack =>
        pack.valueByPath(path) match {
          case Some(v) =>
            formatter(v)
          case None => ""
        }
      case _ => ""
    }

  override def toString = content.toString

  def date(path:String,pattern:String) =
    content match {
      case pack: TStruct#Pack =>
        pack.valueByPath(path) match {
          case Some(date:TDateTime.Pack)  if date.dataType.vtm == TDateTime.vtm =>
            TDateTime.dataPack2String(date)(pattern)
          case None => ""
        }
      case _ => ""
    }

  def node(path: String): Extractor = content match {
    case pack: TStruct#Pack =>
      pack.valueByPath(path) match {
        case Some(pack: TStruct#Pack) =>
          Extractor(pack, formatter)
        case _ => null
      }
    case _ => null
  }

  def list(path: String): List[Extractor] = {
    content match {
      case pack: TStruct#Pack =>
        pack.valueByPath(path) match {
          case Some(list: TList[_]#Pack) =>
            list.map(item => Extractor(item, formatter))
          case Some(v) => Nil
          case None => Nil
        }
      case _ => Nil
    }
  }

  def map(path: String): Map[String, Extractor] = {
    content match {
      case pack: TStruct#Pack =>
        pack.valueByPath(path) match {
          case Some(map: TMap[_, _]#Pack) =>
            map.value.map {
              case (key, item) => key.toString -> Extractor(item, formatter)
            }
          case Some(v) => Map[String, Extractor]()
          case None => Map[String, Extractor]()
        }
      case _ => Map[String, Extractor]()
    }
  }
}

object Extractor {

  type Formatter = PartialFunction[Any, String]

  val defaultFormatter: Formatter = {
    case a => a.toString
  }

}