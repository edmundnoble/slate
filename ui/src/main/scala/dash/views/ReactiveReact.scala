package dash.views

import japgolly.scalajs.react.{CallbackTo, ReactComponentB, TopNode}
import monix.execution.Scheduler
import monix.reactive.Observable

import scalaz.Monoid

object ReactiveReact {
  abstract class ReactiveState[ST, R, C](val reactive: R, val const: C) {
    def setReactive(r: R): ST
  }

  @inline def reactiveBackendReplaceL[P, R, ST <: ReactiveState[ST, R, _], N <: TopNode]
  (builder: ReactComponentB[P, ST, Unit, N], lens: P => Observable[R])
  (implicit sch: Scheduler): ReactComponentB[P, ST, Unit, N] =
    builder.componentWillMount($ =>
      CallbackTo.pure {
        val _ = lens($.props).foreach { r =>
          val () = $.modState(_.setReactive(r)).runNow()
        }
      }
    )

  @inline def reactiveBackendReplace[R, ST <: ReactiveState[ST, R, _], N <: TopNode]
  (builder: ReactComponentB[Observable[R], ST, Unit, N])
  (implicit sch: Scheduler): ReactComponentB[Observable[R], ST, Unit, N] =
    reactiveBackendReplaceL(builder, s => s)

  @inline def reactiveMonoidBackend[R, ST <: ReactiveState[ST, R, _], N <: TopNode]
  (builder: ReactComponentB[Observable[R], ST, Unit, N])
  (implicit R: Monoid[R], sch: Scheduler): ReactComponentB[Observable[R], ST, Unit, N] = {
    builder.componentWillMount { $ =>
      CallbackTo.pure {
        val _ = $.props.foldLeftF(R.zero)(R.append(_, _)).foreach { r =>
          val () = $.modState(_.setReactive(r)).runNow()
        }
      }
    }
  }

}
