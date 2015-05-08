0.1.2 
=====

1.数据类型操作
-------------
例如类型：
    
    object UserInfo extends TStruct with Versioned {   
      val name  Field(TString("name","姓名")
      val age = Field(TInt("age","年龄")
      val title  Field(TString("title","职位")
      val scalaAge = Field(TInt("scalaAge","Scala使用时间")
      val house = Field(TString("house","别墅地址")
      val memo = Field(TString("memo","备注"))
    }
  
  a) := 操作符支持更灵活的表达方案

      val nullString :String = null
      val user1 = UserInfo(
        name := "Simon",                //基础类型赋值
        title := Some("Scala Haker"),   //也可以是Option
        age := 40,
        scalaAge := Some(2),
        house := None,                  //None 不会被赋值到数据中
        memo := nullString              //可以是 null ，不会被赋值到数据中
      )

 b) <~~ 操作符支持更灵活的表达方案
   原来 <~~  List(...)  List中只能使用ValuePack[_],现在可以用Option[ValuePack[_]],可以二者混用

 c) 数据类型转换
 
       Object A extends TStruct {...}
       Object B extends TStruct {...}
       val a:A.Pack = A(...)
       val b:B.Pack = a.to(B)  //将数据类型A的实例转换成B的实例

   完整的例子参见[代码](https://github.com/stepover/zzb/blob/0.1.2/zzb-datatype/src/test/scala/zzb/datatype/StructTransTest.scala)
   
 d) TStruct#Pack 实例的数据字段修改机制变更
    
  原先我们可以使用 TStruct#Pack 的apply函数修改其中一个或多个字段的值并返回一个新的 Pack 实例。如：
    
    val user2 = user1(name:="jack",age := 30)
    
  现在该改为使用 alter 函数，如：
    
    val user2 = user1.alter(name:="jack",age := 30)
 
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
       
4. DomainActor 利用缓存减少数据库访问

DomainActor 中增加两个函数，可以重载控制默认的行为

      //是否每次保存数据时都立即存到数据库
      def alwaysflush = true
    
      //自动保存到数据库的时间间隔(秒)
      def autoFlushIntervalSeconds = 60

DomainFSM 中的doSave函数增加了一个布尔值的 doFlash 参数，控制是否立即更新数据到数据库，不提供这个参数默认使用 alwaysflush

Rest API 中进行数据更新时可以使用  Post(s"/api/planes/$pid/alter/$alterSeq?flush=true")  的方式强制更新到数据库，
不提供这个参数就使用 alwaysflush 的设置。

如果设置了 autoFlushIntervalSeconds ，数据会定时自动更新到数据库。
DomainActor 的 postStop 中也会自动执行更新到数据库的动作
在DomainActor的子类型中也可以直接调用 DomainActor 的 flush 方法同步数据到数据库。