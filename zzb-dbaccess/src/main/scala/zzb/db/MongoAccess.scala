package zzb.db

import com.mongodb.casbah._
import Mongos._
/**
 * mongo操作
 * Created by blackangel on 2014/7/18.
 */
trait MongoAccess {

  def db[A](name:String)(f: MongoDB =>A)={
      f(_db(name))
  }
}
trait ExpressionNode{
  def parent:Some[ExpressionNode]
  def children:List[ExpressionNode]
}
trait LogicalBoolean extends ExpressionNode{

}