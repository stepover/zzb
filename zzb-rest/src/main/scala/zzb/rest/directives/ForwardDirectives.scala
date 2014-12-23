package zzb.rest.directives

import akka.actor.{ ActorContext, ActorRef }
import zzb.rest._
import spray.http.Uri.Path._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 14-3-11
 * Time: 上午8:32
 * Copyright baoxian.com 2012~2020
 */
trait ForwardDirectives {
  def forward(act: ⇒ ActorRef)(implicit context: ActorContext): Route = ctx ⇒ act.forward(ctx)

  def forwardChild(implicit childByName: String ⇒ Either[(StatusCode, String), ActorRef], context: ActorContext): Route = ctx ⇒ {

    ctx.unmatchedPath match {
      case Slash(Segment(head, Slash(tail))) ⇒
        childByName(head) match {
          case Right(actor) ⇒ actor.forward(ctx.copy(unmatchedPath = Slash(tail)))
          case Left(errors) ⇒ ctx.complete(errors._1, errors._2)
        }
      case Slash(Segment(head, Empty)) ⇒
        childByName(head) match {
          case Right(actor) ⇒
            actor.forward(ctx.copy(unmatchedPath = Empty))
          case Left(errors) ⇒ ctx.complete(errors._1, errors._2)
        }
      case _ ⇒ ctx.reject()
    }
  }

  def forwardChildFuture(implicit childByName: String ⇒ Future[Either[(StatusCode, String), ActorRef]], context: ActorContext): Route = ctx ⇒ {

    import context.dispatcher
    ctx.unmatchedPath match {
      case Slash(Segment(head, Slash(tail))) ⇒
        childByName(head).onComplete {
          case Success(Right(actor)) ⇒ actor.forward(ctx.copy(unmatchedPath = Slash(tail)))
          case Success(Left(errors)) ⇒ ctx.complete(errors._1, errors._2)
          case Failure(e)            ⇒ ctx.complete(StatusCodes.InternalServerError, e.getMessage)
        }
      case Slash(Segment(head, Empty)) ⇒
        childByName(head) onComplete {
          case Success(Right(actor)) ⇒
            actor.forward(ctx.copy(unmatchedPath = Empty))
          case Success(Left(errors)) ⇒ ctx.complete(errors._1, errors._2)
          case Failure(e)            ⇒ ctx.complete(StatusCodes.InternalServerError, e.getMessage)
        }
      case _ ⇒ ctx.reject()
    }
  }
}

