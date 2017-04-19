package slate
package app
package refresh

import upickle.Js

sealed abstract class BootRefreshPolicy {
  def shouldRefresh(secondsAge: Int): Boolean
}

object BootRefreshPolicy {
  final case class IfOlderThan(seconds: Int) extends BootRefreshPolicy {
    def shouldRefresh(secondsAge: Int): Boolean = secondsAge > seconds
  }
  case object Never extends BootRefreshPolicy {
    def shouldRefresh(secondsAge: Int): Boolean = false
  }
  case object Always extends BootRefreshPolicy {
    def shouldRefresh(secondsAge: Int): Boolean = true
  }

  import upickle.default._
  implicit val pkl: Reader[BootRefreshPolicy] with Writer[BootRefreshPolicy] =
    new Reader[BootRefreshPolicy] with Writer[BootRefreshPolicy] {
      def read0: PartialFunction[Js.Value, BootRefreshPolicy] = Function.unlift[Js.Value, BootRefreshPolicy] {
        case Js.Obj(("ifOlderThan", Js.Num(secondsAge))) if secondsAge.isValidInt => Some(IfOlderThan(secondsAge.toInt))
        case Js.Obj(("never", Js.Arr())) => Some(Never)
        case Js.Obj(("always", Js.Arr())) => Some(Always)
        case _ => None
      }

      def write0: (BootRefreshPolicy) => Js.Value = { policy: BootRefreshPolicy =>
        Js.Obj(
          policy match {
            case IfOlderThan(seconds) => "ifOlderThan" -> Js.Num(seconds)
            case Never => "never" -> Js.Arr()
            case Always => "always" -> Js.Arr()
          }
        )
      }
    }
}

