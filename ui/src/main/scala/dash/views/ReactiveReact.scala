package dash.views

import japgolly.scalajs.react.CompState.{ReadCallbackWriteCallbackOps, ReadDirectWriteCallbackOps, ReadDirectWriteDirectOps, WriteOps}
import japgolly.scalajs.react.{Callback, CompState, ReactComponentB, TopNode}
import monix.execution.Scheduler
import monix.reactive.Observable

import scalaz.{Monoid, Semigroup}

object ReactiveReact {
  abstract class ReactiveState[ST, R, C](val reactive: R, val const: C) {
    def setReactive(r: R): ST
  }

  @inline def reactiveBackendReplaceL[P, R, ST <: ReactiveState[ST, R, _], N <: TopNode]
  (builder: ReactComponentB[P, ST, Unit, N], lens: P => Observable[R])(implicit sch: Scheduler): ReactComponentB[P, ST, Unit, N] =
    builder.componentWillMount($ =>
      Callback {
        lens($.props).foreach { r =>
          val () = $.modState(_.setReactive(r)).runNow()
        }
      }
    )

  @inline def reactiveBackendReplace[R, ST <: ReactiveState[ST, R, _], N <: TopNode]
  (builder: ReactComponentB[Observable[R], ST, Unit, N])(implicit sch: Scheduler): ReactComponentB[Observable[R], ST, Unit, N] =
    reactiveBackendReplaceL(builder, identity(_))

  @inline def reactiveMonoidBackend[P, R, ST <: ReactiveState[ST, R, _], N <: TopNode]
  (builder: ReactComponentB[P, ST, Unit, N])()(implicit ev: P =:= Observable[R], R: Monoid[R], sch: Scheduler): ReactComponentB[P, ST, Unit, N] = {
    builder.componentWillMount($ =>
      Callback {
        $.props.foldLeftF(R.zero)(R.append(_, _)).foreach { r =>
          val () = $.modState(_.setReactive(r)).runNow()
        }
      })
  }

}
