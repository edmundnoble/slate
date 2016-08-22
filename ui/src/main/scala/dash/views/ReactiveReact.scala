package dash.views

import japgolly.scalajs.react.{CallbackTo, ReactComponentB, TopNode}
import monix.execution.Scheduler
import monix.reactive.Observable

import scalaz.Monoid

object ReactiveReact {
  abstract class ReactiveState[ST, R, C](val reactive: R, val const: C) {
    def setReactive(r: R): ST
  }

  implicit class ReactiveOps[P, ST, B, N <: TopNode](val builder: ReactComponentB[P, ST, B, N]) {

    @inline final def reactiveReplaceL[R]
    (builder: ReactComponentB[P, ST, B, N], getReactivePart: P => Observable[R])
    (implicit sch: Scheduler,
     stateIsReactiveEv: ST <:< ReactiveState[ST, R, _]): ReactComponentB[P, ST, B, N] =
      builder.componentWillMount($ =>
        CallbackTo.pure {
          val _ = getReactivePart($.props).foreach { r =>
            val () = $.modState(_.setReactive(r)).runNow()
          }
        }
      )

    @inline final def reactiveReplace[R]
    (implicit sch: Scheduler,
     stateIsReactiveEv: ST <:< ReactiveState[ST, R, _], propsAreReactiveEv: P =:= Observable[R]
    ): ReactComponentB[P, ST, B, N] =
      reactiveReplaceL(builder, propsAreReactiveEv)

    @inline final def reactiveMonoid[R]
    (implicit sch: Scheduler, R: Monoid[R],
     stateIsReactiveEv: ST <:< ReactiveState[ST, R, _], propsAreReactiveEv: P =:= Observable[R]
    ): ReactComponentB[P, ST, B, N] = {
      builder.componentWillMount { $ =>
        CallbackTo {
          val _ = $.props.foldLeftF(R.zero)(R.append(_, _)).foreach { r =>
            val () = $.modState(_.setReactive(r)).runNow()
          }
        }
      }
    }
  }

}
