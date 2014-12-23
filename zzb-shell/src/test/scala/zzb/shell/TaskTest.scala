package zzb.shell

import org.scalatest.MustMatchers
import org.scalatest.WordSpec
import java.io.{ PrintStream, ByteArrayOutputStream }
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-12
 * Time: 下午2:29
 * Copyright baoxian.com 2012~2020
 */
/*
  测试 Task 可以正常执行
 */
class TaskTest extends WordSpec with MustMatchers {

  Shell.init(ConfigFactory.load("defaultShellRemote.conf"),ActorSystem())


  "default Task " must {
    " 'tasks'  be found  " in {
      Task.taskInfo("tasks").className must equal("zzb.shell.task.Tasks")

    }

  }

  "Task Hello" must {

    "print 'hello' " in {
      Task.parseOne("hello", "zzb.shell.Hello")
      val bs = new ByteArrayOutputStream()
      val ps = new PrintStream(bs)

      Task("hello", ps, ps,null, Nil)
      Thread.sleep(100)
      val result = bs.toString

      result must equal("hello")
    }

  }

  "Task Parser" must {
    "can find task by shortName or fullName" in {

      Task.parseOne("a1.b1.c1", "wash.shell.Hello1")
      Task.taskInfo("c1").space must equal("a1.b1")
      Task.taskInfo("c1") must equal(Task.taskInfo("a1.b1.c1"))

    }

    "can add Task to default space" in {
      Task.parseOne("d1", "wash.shell.Hello!")
      Task.taskInfo("d1").space must equal(Task.defaultSpace)
    }
    "can throw TaskNotFoundException when request a not exist task" in {
      intercept[TaskNotFoundException] {
        Task.taskInfo("no-this-task")
      }
    }
    "can throw TaskDuplicateException when parse same name " in {
      intercept[TaskDuplicateException] {
        Task.parseOne("a1.b1.c1", "wash.shell.Hello2")
      }
    }
    "can throw TaskDuplicateException when found same short name task " in {
      Task.parseOne("a2.b2.c1", "wash.shell.Hello3")
      intercept[TaskDuplicateException] {

        Task.taskInfo("c1")
      }
    }
    "can throw TaskDuplicateException when found same short name task,one in default space " in {
      Task.parseOne("c2", "wash.shell.Hello77")
      Task.parseOne("a2.b2.c2", "wash.shell.Hello66")

      intercept[TaskDuplicateException] {
        Task.taskInfo("c2")
      }
    }
    "can save the task description" in {
      Task.parseOne("about", "wash.main.task.About  ;  显示 about 信息.  ")
      val info = Task.taskInfo("about")
      info.desc must equal("显示 about 信息.")
    }
  }



}

class Hello extends Task {
  print("hello")

}

