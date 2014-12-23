package zzb.storage

import com.mongodb.casbah.{query, Imports}
import com.mongodb.casbah.query.dsl.QueryExpressionObject
import  com.mongodb.casbah.query.Imports._
import com.typesafe.scalalogging.slf4j.Logging
import zzb.datatype.StructPath

/**
 * todo:
 * @author 何伟波
 * @since  0.1.0
 */
trait DBObjectHelper extends Logging{

  val  gtStringMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gt") if value.isInstanceOf[String]=> Some( key $gt value.toString )
  }
  val  gtDoubleMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gt") if value.isInstanceOf[Double]=> Some( key $gt value.asInstanceOf[Double] )
  }
  val  gtIntMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gt") if value.isInstanceOf[Int]=> Some( key $gt value.toString.toDouble )
  }
  val  gtBooleanMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gt") if value.isInstanceOf[Boolean]=> Some( key $gt value.toString.toBoolean )
  }
  val  ltStringMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lt") if value.isInstanceOf[String]=> Some( key $lt value.toString )
  }
  val  ltDoubleMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lt") if value.isInstanceOf[Double]=> Some( key $lt value.asInstanceOf[Double] )
  }
  val  ltIntMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lt") if value.isInstanceOf[Int]=> Some( key $lt value.toString.toDouble )
  }
  val  ltBooleanMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lt") if value.isInstanceOf[Boolean]=> Some( key $lt value.toString.toBoolean )
  }
  val  gteStringMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gte") if value.isInstanceOf[String]=> Some( key $gte value.toString )
  }
  val  gteDoubleMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gte") if value.isInstanceOf[Double]=> Some( key $gte value.asInstanceOf[Double] )
  }
  val  gteIntMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gte") if value.isInstanceOf[Int]=> Some( key $gte value.toString.toDouble )
  }
  val  gteBooleanMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"gte") if value.isInstanceOf[Boolean]=> Some( key $gte value.toString.toBoolean )
  }
  val  lteStringMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lte") if value.isInstanceOf[String]=> Some( key $lte value.toString )
  }
  val  lteDoubleMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lte") if value.isInstanceOf[Double]=> Some( key $lte value.asInstanceOf[Double] )
  }
  val  lteIntMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lte") if value.isInstanceOf[Int]=> Some( key $lte value.toString.toDouble )
  }
  val  lteBooleanMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"lte") if value.isInstanceOf[Boolean]=> Some( key $lte value.toString.toBoolean )
  }
  val  eqStringMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"eq") if value.isInstanceOf[String]=> Some( key $eq value.toString )
  }
  val  eqDoubleMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"eq") if value.isInstanceOf[Double]=> Some( key $eq value.asInstanceOf[Double] )
  }
  val  eqIntMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"eq") if value.isInstanceOf[Int]=> Some( key $eq value.toString.toDouble )
  }
  val  eqBooleanMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case (key,value,"eq") if value.isInstanceOf[Boolean]=> Some( key $eq value.toString.toBoolean )
  }
  val  otherMethod :PartialFunction[(String,Any,String),Option[Imports.DBObject with QueryExpressionObject]]={
    case _=> None
  }

  val gtfilterMethod= gtStringMethod orElse gtDoubleMethod orElse gtIntMethod  orElse gtBooleanMethod
  val gtefilterMethod=gteStringMethod orElse gteDoubleMethod orElse gteIntMethod  orElse gteBooleanMethod
  val ltfilterMethod=ltStringMethod orElse ltDoubleMethod orElse ltIntMethod    orElse ltBooleanMethod
  val ltefilterMethod=lteStringMethod orElse lteDoubleMethod orElse lteIntMethod     orElse lteBooleanMethod
  val eqfilterMethod=eqStringMethod orElse eqDoubleMethod orElse eqIntMethod      orElse eqBooleanMethod
  val dbObjectFilter= gtfilterMethod orElse gtefilterMethod orElse ltfilterMethod orElse ltefilterMethod orElse eqfilterMethod orElse otherMethod

  def structPathToString(structPath:StructPath)=
    structPath.relativeStr.drop(1).replace("/",".")

  /**
   * 创建and的查询条件
   * @param params
   * @return
   */
  def makeAndFilter(params:Imports.DBObject *): query.Imports.DBObject ={
    $and(params: _*)
  }

  /**
   * 创建and的查询条件
   * @param params
   * @return
   */
  def makeAndFilter(params:List[Imports.DBObject]): query.Imports.DBObject ={
    makeAndFilter(params: _*)
  }

  /**
   * 创建or的查询条件
   * @param params
   * @return
   */
  def makeOrFilter(params:Imports.DBObject *): query.Imports.DBObject ={
    $or(params: _*)
  }

  /**
   * 创建or的查询条件
   * @param params
   * @return
   */
  def makeOrFilter(params:List[Imports.DBObject]): query.Imports.DBObject ={
    makeOrFilter(params: _*)
  }

  /**
   * 根据条件去生成查询条件
   * @param key
   * @param value
   * @param option  eq:相等，lt：小于， lte：小于等于 ， gt：大于 ,gte:大于等于 ,
   * @return
   */
  def makeFilterByConditionName(key:String , value:Any,option:String): Option[query.Imports.DBObject]={
        dbObjectFilter(key,value,option)
  }

  /**
   * 根据条件去生成查询条件
   * @param structPath
   * @param value
   * @param option
   * @return
   */
  def makeFilterByConditionName(structPath:StructPath , value:Any,option:String): Option[query.Imports.DBObject]={
        val key = structPathToString(structPath)
        dbObjectFilter(key,value,option)
  }

  /**
   * 创建小于的条件
   * @param key
   * @param value
   * @return
   */
  def makeLtFilter(key:String , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(key,value,"lt")
  }

  /**
   * 创建小于的条件
   * @param structPath
   * @param value
   * @return
   */
  def makeLtFilter(structPath:StructPath , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(structPath,value,"lt")
  }

  /**
   * 创建小于等于的条件
   * @param key
   * @param value
   * @return
   */
  def makeLteFilter(key:String , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(key,value,"lte")
  }

  /**
   * 创建小于等于的条件
   * @param structPath
   * @param value
   * @return
   */
  def makeLteFilter(structPath:StructPath , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(structPath,value,"lte")
  }
  /**
   * 创建大于的条件
   * @param key
   * @param value
   * @return
   */
  def makeGtFilter(key:String , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(key,value,"gt")
  }

  /**
   * 创建大于的条件
   * @param structPath
   * @param value
   * @return
   */
  def makeGtFilter(structPath:StructPath , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(structPath,value,"gt")
  }

  /**
   * 创建大于等于的条件
   * @param key
   * @param value
   * @return
   */
  def makeGteFilter(key:String , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(key,value,"gte")
  }

  /**
   * 创建大于等于的条件
   * @param structPath
   * @param value
   * @return
   */
  def makeGteFilter(structPath:StructPath , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(structPath,value,"gte")
  }

  /**
   * 创建等于的条件
   * @param key
   * @param value
   * @return
   */
  def makeEqFilter(key:String , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(key,value,"eq")
  }

  /**
   * 创建等于的条件
   * @param structPath
   * @param value
   * @return
   */
  def makeEqFilter(structPath:StructPath , value:Any): Option[query.Imports.DBObject]={
    makeFilterByConditionName(structPath,value,"eq")
  }
}
