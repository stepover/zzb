package zzb.shell

/**
 * Created with IntelliJ IDEA.
 * User: Simon Xiao
 * Date: 13-9-13
 * Time: 下午5:32
 * Copyright baoxian.com 2012~2020
 */
/**
 * This interface is for classes that want to be aware of entering
 * and leaving each command loop.
 *
 * You might want some special resources to be allocated before CLI
 * starts; after conversation you want to free those resources.
 * By implementing this interface you get the ability to handle these
 * events.
 *
 * Note that since Shell can possibly have other means of operation
 * instead of commandLoop(), these methods may be not called.
 *
 * @author ASG
 */
trait ShellManageable {
  /**
   * This method is called when it is about to enter the command loop.
   */
  def cliEnterLoop

  /**
   * This method is called when Shell is leaving the command loop.
   */
  def cliLeaveLoop
}