package zzb.datatype

import scala.reflect._

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-11-23
 * Time: 下午3:59
 * Copyright baoxian.com 2012~2020
 */
trait TFunc0[R] extends TMono[() =>R ]{
  val vtm =  classTag[() =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc1[T1,R] extends TMono[T1 => R]{
  val vtm =  classTag[T1 => R]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc2[T1,T2,R] extends TMono[(T1,T2) => R] {
  val vtm =  classTag[(T1,T2) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc3[T1,T2,T3,R] extends TMono[(T1,T2,T3) => R] {
  val vtm =  classTag[(T1,T2,T3) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()

}

trait TFunc4[T1,T2,T3,T4,R] extends TMono[(T1,T2,T3,T4) => R] {
  val vtm =  classTag[(T1,T2,T3,T4) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc5[T1,T2,T3,T4,T5,R] extends TMono[(T1,T2,T3,T4,T5) => R]{
  val vtm =  classTag[(T1,T2,T3,T4,T5) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc6[T1,T2,T3,T4,T5,T6,R] extends TMono[(T1,T2,T3,T4,T5,T6) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc7[T1,T2,T3,T4,T5,T6,T7,R] extends TMono[(T1,T2,T3,T4,T5,T6,T7) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6,T7) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc8[T1,T2,T3,T4,T5,T6,T7,T8,R] extends TMono[ (T1,T2,T3,T4,T5,T6,T7,T8) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6,T7,T8) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc9[T1,T2,T3,T4,T5,T6,T7,T8,T9,R] extends TMono[(T1,T2,T3,T4,T5,T6,T7,T8,T9) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6,T7,T8,T9) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,R] extends TMono[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}
trait TFunc11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,R] extends TMono[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}

trait TFunc12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,R] extends TMono[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) => R] {
  val vtm =  classTag[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) =>R ]
  def parse(str: String):Pack = throw new UnsupportedOperationException()
}


