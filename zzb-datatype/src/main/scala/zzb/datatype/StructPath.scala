package zzb.datatype

import scala.reflect._

sealed abstract class StructPath(val inStructPath: NestedStructFields) {
  def getDomainData[T <: TStruct#Pack](doc: T): Option[Any]

  def targetType: DataType[Any]

  override def toString: String = if (inStructPath.size == 0) "/" else inStructPath

  val relativeStr: String = if (inStructPath.size == 0) "" else inStructPath.tail

  def contains(other: StructPath): Boolean = other.toString.startsWith(this.toString)

  def ::(field :StructField) : StructPath

  def regexContains(other: StructPath): Boolean ={
    val x = other.toString
    val y = this.toString
    val pattern = y.r
    pattern.findFirstIn(other.toString) match {
      case Some(v) if v == x => true
      case _ => false
    }
  }

  def alterDomainData[T <: TStruct#Pack](doc: T, newData: Option[ValuePack[Any]],
                                         merge: MergeManner.Value = MergeManner.UseNew): T

  protected def alterDocField[T <: TStruct#Pack](oldDoc: T,
                                                 sections: NestedStructFields,
                                                 newData: Option[ValuePack[Any]],
                                                 merge: MergeManner.Value): ValuePack[Any] = {
    sections match {
      case head :: Nil =>
        val nodeType = head()
        val oldValue = oldDoc(nodeType)
        if (newData.isDefined) {
          val nv =
            if (newData.get.dataType == nodeType) newData.get
            else {
              if (newData.get.dataType.vtm == nodeType.vtm)
                nodeType.AnyToPack(newData.get).getOrElse(
                  throw new RuntimeException("data type miss match")
                )
              else
                throw new RuntimeException("data type miss match")
            }
          (oldValue, merge) match {
            case (Some(ov), MergeManner.UseNew) =>

              oldDoc <~: (nv ->> ov)
            case (Some(ov), MergeManner.UseOld) => oldDoc <~: (ov ->> nv)
            case (_, _) =>
              oldDoc <~ newData.get
          }
        } else {
          (oldValue, merge) match {
            case (None, MergeManner.Replace) => oldDoc //原来也没数据，新数据也是 None ,无操作
            case (Some(ov), MergeManner.Replace) => oldDoc - nodeType //原来有数据，新数据不是 None ,清除原数据
            case (_, _) => oldDoc
          }
        }
      //路径未到头
      case head :: tail =>
        //取出这一段的类型
        val nodeType: DataType[Any] = head()
        val ofv = oldDoc(nodeType) //取出文档这一段的数据
        ofv match {
          case None => //如果没有数据则尝试创建缺省值
            val nd = nodeType match {
              case structType: TStruct =>
                Some(structType())
              case _ => None
            }
            nd match {
              case Some(newNodeDoc) => oldDoc <~: alterDocField(newNodeDoc, tail, newData, merge)
              case None => oldDoc
            }
          case Some(d: TStruct#Pack) => oldDoc <~: alterDocField(d, tail, newData, merge) //有数据就继续
          case Some(d) => throw new Exception("error")
        }
    }
  }
}

object StructPath {

  //用来获取列表长度时使用的特定idx
  final val ListSizeIdx = -10000
  //用来获取Map长度时使用的特定idx
  final val MapSizeKey = "@size"

   final val StringType = classTag[String]

  def apply(inStructPath: NestedStructFields) = NodePath(inStructPath)

  def apply(inStructPath: NestedStructFields, idx: Int) = ListPath(inStructPath, idx)

  def apply(inStructPath: NestedStructFields, key: String) = MapPath(inStructPath, key)

  implicit def inStructPath2NodePath(inStructPath: NestedStructFields) = apply(inStructPath)

  implicit def dtToPath(dt: DataType[Any]): NodePath = NodePath(dt.path)

  implicit def dtFuncTurpToPathTurp(dtFun1: () => DataType[Any], dtFun2: () => DataType[Any]): (NodePath, NodePath) =
    (NodePath(dtFun1().path), NodePath(dtFun2().path))

  implicit def dtFunToPath(dtFun: () => DataType[Any]): NodePath = NodePath(dtFun().path)

  implicit def dtIdxToPath(dtIdx: (DataType[Any], Int)): ListPath = ListPath(dtIdx._1.path, dtIdx._2)

  implicit def dtFunIdxToPath(dtFunIdx: (() => DataType[Any], Int)): ListPath = ListPath(dtFunIdx._1().path, dtFunIdx._2)

  implicit def dtKeyToPath(dtIdx: (DataType[Any], String)): MapPath = MapPath(dtIdx._1.path, dtIdx._2)

  implicit def dtKeyFuncToPath(dtIdx: (() => DataType[Any], String)): MapPath = MapPath(dtIdx._1().path, dtIdx._2)
}

case class NullStructNodeDataException(msg: String) extends Exception(msg)

case class NodePath(override val inStructPath: NestedStructFields) extends StructPath(inStructPath) {
  override def getDomainData[T <: TStruct#Pack](doc: T): Option[Any] = {
    val nodeType = inStructPath.through
    doc(nodeType) match {
      case Some(v) => Some(v)
      case None =>
        nodeType match {
          case mapType: TMap[_, _] =>
            Some(mapType.apply(Map())) //Map类型可以给个空Map
          case listType: TList[_] =>
            Some(listType.apply(Nil)) //List类型可以给个空List
          case _ => None
        }
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case np: NodePath => this.toString.equals(np.toString)
      case np: NestedStructFields => this.toString.equals(NodePath(np).toString)
      case _ => false
    }
  }

  override def targetType: DataType[Any] = inStructPath.through

  override def
  alterDomainData[T <: TStruct#Pack](doc: T,
                                     newData: Option[ValuePack[Any]],
                                     merge: MergeManner.Value = MergeManner.UseNew): T = {
    inStructPath match {
      case Nil => throw new Exception("error")
      case h :: Nil =>
        val newDoc = (newData, merge) match {
          case (Some(nd), MergeManner.UseNew) => nd ->> doc
          case (Some(nd), MergeManner.UseOld) => doc ->> nd
          case (Some(nd), MergeManner.Replace) => nd
          case (None, MergeManner.UseNew) => doc
          case (None, MergeManner.UseOld) => doc
          case (_, _) => throw new NullStructNodeDataException("None Struct Data")
        }
        newDoc.asInstanceOf[T]
      case h :: tail =>
        alterDocField(doc, tail, newData, merge).asInstanceOf[T]
    }
  }

  override def ::(field: StructField): StructPath = NodePath(field :: inStructPath)
}

case class ListPath(override val inStructPath: NestedStructFields, idx: Int) extends StructPath(inStructPath) {

  override def toString = super.toString + "/" + idx

  override def ::(field: StructField): StructPath = ListPath(field :: inStructPath,idx)

  override def getDomainData[T <: TStruct#Pack](doc: T): Option[Any] =

    doc(inStructPath.through) match {
      case Some(pack) => pack match {
        case listPack: TPackList[_]#Pack if idx == StructPath.ListSizeIdx =>
          Some(listPack.length)
        case listPack: TPackList[_]#Pack if idx < 0 => Some(listPack) //索引小于0返回整个列表
        case listPack: TPackList[_]#Pack if idx >= listPack.length => None
        case listPack: TPackList[_]#Pack => Some(listPack(idx))
        case _ => None
      }
      case None => None
    }


  override def targetType: DataType[Any] = {
    inStructPath.through match {
      case listType: TPackList[_] => listType.itemDataType
      case listType: TList[_] =>
        if (listType.lm == StructPath.StringType)   TString
        else
          listType.lm.runtimeClass match {
            case java.lang.Byte.TYPE => TByte
            case java.lang.Short.TYPE => TShort
            case java.lang.Integer.TYPE => TInt
            case java.lang.Long.TYPE => TLong
            case java.lang.Float.TYPE => TFloat
            case java.lang.Double.TYPE => TDouble
            case java.lang.Boolean.TYPE => TBoolean
            case _ =>
              throw RestApiMustUseZzbDataType(s"${listType.toString} wrong 'List' type define,can't be used in rest api")
          }
      case dt => throw RestApiMustUseZzbDataType(s"${dt.toString} wrong 'List' type define,can't be used in rest api")
    }
  }

  override def
  alterDomainData[T <: TStruct#Pack](doc: T,
                                     newData: Option[ValuePack[Any]],
                                     merge: MergeManner.Value = MergeManner.UseNew): T = {
    val listType = inStructPath.through
    if (newData.isEmpty) {
      //删除操作
      doc(listType) match {
        case Some(pack) => pack match {
          case listPack: TPackList[_]#Pack if idx >= listPack.length || idx < 0 =>
            throw new ArrayIndexOutOfBoundsException(idx)
          case listPack: TPackList[_]#Pack =>
            val newListPack = listPack.removeItem(idx)
            alterDocField(doc, inStructPath.tail, Some(newListPack), merge).asInstanceOf[T]
          case _ => throw new RuntimeException("invalid data access")
        }
        case None => throw new RuntimeException("invalid data access")
      }
    } else {
      doc(listType) match {
        case Some(pack) => pack match {
          case listPack: TPackList[_]#Pack if idx >= listPack.length =>
            throw new ArrayIndexOutOfBoundsException(idx)
          case listPack: TPackList[_]#Pack if idx < 0 => //新增
            val newListPack = listPack.addItem(newData.get)
            alterDocField(doc, inStructPath.tail, Some(newListPack), merge).asInstanceOf[T]
          case listPack: TPackList[_]#Pack =>
            val newListPack = newData.get match {
              case newItem: TStruct#Pack =>
                val oldItem = listPack.getItem(idx).asInstanceOf[TStruct#Pack]
                merge match {
                  case MergeManner.Replace =>
                    listPack.setItem(idx, newItem)
                  case MergeManner.UseNew =>
                    listPack.setItem(idx, newItem ->> oldItem)
                  case MergeManner.UseOld =>
                    listPack.setItem(idx, oldItem ->> newItem)
                }
              case _ => listPack.setItem(idx, newData.get)
            }
            alterDocField(doc, inStructPath.tail, Some(newListPack), MergeManner.UseNew).asInstanceOf[T]
          case _ => throw new RuntimeException("invalid data access")
        }
        case None if idx < 0 =>
          val listPack = listType.asInstanceOf[TPackList[_]](Nil)
          val newListPack = listPack.addItem(newData.get)
          alterDocField(doc, inStructPath.tail, Some(newListPack), merge).asInstanceOf[T]
        case None => throw new RuntimeException("invalid data access")
      }
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case np: ListPath => this.toString.equals(np.toString)
      case _ => false
    }
  }
}

case class MapPath(override val inStructPath: NestedStructFields, key: String) extends StructPath(inStructPath) {

  override def toString = super.toString + "/" + key

  override def ::(field: StructField): StructPath = MapPath(field :: inStructPath,key)

  override def getDomainData[T <: TStruct#Pack](doc: T): Option[Any] =
    doc(inStructPath.through) match {
      case Some(pack) => pack match {
        case mapPack: TStrKeyPackMap[_]#Pack =>
          if (key == StructPath.MapSizeKey)
            Some(mapPack.size)
          else
            mapPack(key)
        case _ => None
      }
      case None => None
    }

  override def targetType: DataType[Any] = {
    inStructPath.through match {
      case mapType: TStrKeyPackMap[_] => mapType.valueDataType
      case mapType: TStrKeyMap[_] =>
        if (mapType.vm == StructPath.StringType)   TString
        else
          mapType.vm.runtimeClass match {
            case java.lang.Byte.TYPE => TByte
            case java.lang.Short.TYPE => TShort
            case java.lang.Integer.TYPE => TInt
            case java.lang.Long.TYPE => TLong
            case java.lang.Float.TYPE => TFloat
            case java.lang.Double.TYPE => TDouble
            case java.lang.Boolean.TYPE => TBoolean
            case _ =>
              throw RestApiMustUseZzbDataType(s"${mapType.toString} wrong 'Map' type define,can't be used in rest api")
          }
      case dt => throw RestApiMustUseZzbDataType(s"${dt.toString} wrong 'Map' type define,can't be used in rest api")
    }
  }

  override def
  alterDomainData[T <: TStruct#Pack](doc: T,
                                     newData: Option[ValuePack[Any]],
                                     merge: MergeManner.Value = MergeManner.UseNew): T = {

    val mapType = inStructPath.through
    if (newData.isEmpty) {
      //执行删除动作
      doc(mapType) match {
        case Some(pack) => pack match {
          case mapPack: TStrKeyPackMap[_]#Pack =>
            val newMapPack = mapPack.subWith(key)
            alterDocField(doc, inStructPath.tail, Some(newMapPack), merge).asInstanceOf[T]
          case _ => throw new RuntimeException("invalid data access")
        }
        case None => doc
      }
    } else {
      doc(mapType) match {
        case Some(pack) => pack match {
          case mapPack: TStrKeyPackMap[_]#Pack =>
            val newMapPack = newData.get match {
              case _ if !mapPack.contains(key) =>
                mapPack.plusWithAny((key, newData.get))
              case newItem: TStruct#Pack =>
                val oldItem = mapPack.get(key).get.asInstanceOf[TStruct#Pack]
                merge match {
                  case MergeManner.Replace =>
                    mapPack.plusWithAny(key, newItem)
                  case MergeManner.UseNew =>
                    mapPack.plusWithAny(key, newItem ->> oldItem)
                  case MergeManner.UseOld =>
                    mapPack.plusWithAny(key, oldItem ->> newItem)
                }
              case newItem =>
                mapPack.plusWithAny((key, newItem))
            }
            alterDocField(doc, inStructPath.tail, Some(newMapPack), MergeManner.UseNew).asInstanceOf[T]
          case _ => throw new RuntimeException("invalid data access")
        }
        case None =>
          val mapPack = mapType.asInstanceOf[TStrKeyPackMap[_]](Map())
          val newMapPack = mapPack.plusWithAny((key, newData.get))
          alterDocField(doc, inStructPath.tail, Some(newMapPack), merge).asInstanceOf[T]
      }
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case np: MapPath => this.toString.equals(np.toString)
      case _ => false
    }
  }
}

case class RestApiMustUseZzbDataType(msg: String) extends Exception(msg)

object MergeManner extends Enumeration {
  val UseNew = Value(0, "usenew")
  val UseOld = Value(1, "useold")
  val Replace = Value(2, "replace")

  def fromString(merge: Option[String]) = {
    merge match {
      case None => MergeManner.UseNew
      case Some(ms) if MergeManner.values.exists(_.toString == ms.toLowerCase) => MergeManner.withName(ms.toLowerCase)
      case Some(ms) => MergeManner.UseNew
    }
  }
}


