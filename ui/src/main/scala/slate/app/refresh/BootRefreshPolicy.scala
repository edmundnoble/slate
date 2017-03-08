package slate
package app
package refresh

sealed abstract class BootRefreshPolicy {
  def shouldRefresh(secondsAge: Int): Boolean
}

object BootRefreshPolicy {
  final case class IfOlderThan(seconds: Int) extends BootRefreshPolicy {
    override def shouldRefresh(secondsAge: Int): Boolean = secondsAge > seconds
  }
  case object Never extends BootRefreshPolicy {
    override def shouldRefresh(secondsAge: Int): Boolean = false
  }
  case object Always extends BootRefreshPolicy {
    override def shouldRefresh(secondsAge: Int): Boolean = true
  }
}

