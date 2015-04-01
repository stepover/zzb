0.1.2 
=====

1.数据类型操作
-------------
  a) := 操作符支持更灵活的表达方案

      val nullString :String = null
      val userInfo1 = UserInfo(
        userName := "Simon",                //基础类型赋值
        userTitle := Some("Scala Haker"),   //也可以是Option
        userAge := 40,
        scalaAge := Some(2),
        house := None,                      //None 不会被赋值到数据中
        memo := nullString                  //可以是 null ，不会被赋值到数据中
      )

 b) <~~ 操作符支持更灵活的表达方案
   原来 <~~  List(...)  List中只能使用ValuePack[_],现在可以用Option[ValuePack[_]],可以二者混用

 c) 数据类型转换
 
       Object A extends TStruct {...}
       Object B extends TStruct {...}
       val a:A.Pack = A(...)
       val b:B.Pack = a.to(B)  //将数据类型A的实例转换成B的实例

   完整的例子参见[代码](https://github.com/stepover/zzb/blob/0.1.2/zzb-datatype/src/test/scala/zzb/datatype/StructTransTest.scala)

2.改变 zzb-storage 版本控制机制
-----------------------------
  原来每次存储动作都会新建一个版本实例，导致无效版本数量大量增长。
  0.1.2 版本数据存储时 save 增加一个 tag:String 参数,保存时有tag才会建新版本，否则只会升级版本号，旧版本数据被覆盖。
  VersionInfo 数据结构中新增 tag 和 eqtag 字段，tag 是数据的版本，eqtag 表示当前数据和哪个tag 是等价的。

3. zzb-storage 存储驱动增加log 支持
-----------------------------------
   可以在创建存储器的时候指定 LoggingAdapter 
   参见 [代码](https://github.com/stepover/zzb/blob/0.1.2/zzb-domain/src/test/scala/zzb/domain/plane/PlaneSetActor.scala)  第22行。

       val storage: Storage[String, TString, Plane.type] =
       memoryStorage[String, TString, Plane.type](Plane,log = this.log)