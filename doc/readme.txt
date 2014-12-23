zzb 工程介绍

zzb 是基于Scala-Akka-Spray 的Restful服务器架构，各模块功能简介如下。


zzb-box
    这是一个服务容器模块，所有业务服务都按照zzb-box的标准写的话，就可以把多个服务放到一个容器进程中，
    也可以部署到不同的进程中。详情请参阅 “Srv-Box User Guide” 和 “SrvBox 工程规范”两个文档。

zzb-config
    这是对 typesafe config 的简单包装，主要是为了解决不同运行环境中（开发，业务测试，生产系统）配置
    文件装载的问题。参阅“SrvBox 工程规范”

zzb-dbaccess
    这是一个对squeryl的一个很简单的扩展，主要是方便进行连接池的配置和管理，方便同时连接不同的数据库。

zzb-datatype
    这是一个数据类型定义的核心库，用于定义层次化业务数据结构，通过其中的类型元信息能够完成更多复杂的操作。

zzb-storage
    这是为 zzb-datatype 数据类型设计的存储框架，主要目的是提供nosql的存储包装，并支持文档的版本化。

zzb-rest
    这是对 spray 的一个抄袭式改写，主要用来实现非http的Rest架构。基于“面向资源的设计”让每一个业务对象
    都是一个有url的Rest资源，对应于一个Actor。其他服务可以直接使用Akka Remote访问。

zzb-domain
    为基于 zzb-datatype 定义的业务对象提供自动的 rest api 访问机制，既可以通过zzb-rest提供的非akka
    方式访问，也可以很容易的映射成 http rest api, 同时提供了业务对象的状态机支持。

zzb-shell
    这是一个命令行shell 工具库，可以实现自己的 shell 指令。也可以在应用服务中集成 shell Daemon（Zzb-Box中已集成），
    然后可以远程连接到应用服务，通过shell窗口执行服务器预定义的指令。实现对服务的远程查看和管理。

zzb-util
    一些工具，基本没啥东西。



