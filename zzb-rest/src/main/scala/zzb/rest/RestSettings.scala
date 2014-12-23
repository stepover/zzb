package zzb.rest

import java.util.concurrent.TimeUnit

import com.typesafe.config.Config
import akka.actor.ActorRefFactory
import spray.util._

import scala.concurrent.duration.FiniteDuration

/**
 * Created by Simon on 2014/7/12
 */
case class RestSettings(verboseErrorMessages: Boolean,
                        users: Config,
                        renderVanityFooter: Boolean)

object RestSettings extends SettingsCompanion[RestSettings]("zzb.rest") {
  def fromSubConfig(c: Config) = apply(
    c getBoolean "verbose-error-messages",
    c getConfig "users",
    c getBoolean "render-vanity-footer")

  implicit def default(implicit refFactory: ActorRefFactory) =
    apply(actorSystem)
}

