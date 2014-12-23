package zzb.shell.util

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-12-10
 * Time: 上午8:44
 * Copyright baoxian.com 2012~2020
 */

/** 用于分页操作，每一页出了本页的内容外，还有下一页的 future */
case class Page[T](pageIdx: Int, pageSize: Int, content: Seq[T], total: Long, next: Option[Future[Page[T]]])

object Page {

  case class Content[T](content: Seq[T], total: Long)

  /**
   * 获取分页内容的递归算法
   * @param firstPageIdx 第一页的索引
   * @param pageSize   页面大小
   * @param passOver   已经获取过的内容长度
   * @param pageFunc   页面读取函数（该函数的第一个参数是 要读取的页面索引，第二个参数是页面大小）
   * @param execctx    执行上下文
   * @tparam T
   * @return
   */
  def nextPage[T](firstPageIdx: Int, pageSize: Int, passOver: Int,
                  pageFunc: (Int, Int) => Content[T])(implicit execctx: ExecutionContext): Page[T] = {

    val contentFirst = pageFunc(firstPageIdx, pageSize)

    if (contentFirst.content.size + passOver >= contentFirst.total || contentFirst.content.size == 0)
      Page(firstPageIdx, pageSize, contentFirst.content, contentFirst.total, None)
    else
      Page(firstPageIdx, pageSize, contentFirst.content, contentFirst.total, Some(
        Future {
          nextPage(firstPageIdx + 1, pageSize, passOver + contentFirst.content.size, pageFunc)
        }
      )
      )
  }
}