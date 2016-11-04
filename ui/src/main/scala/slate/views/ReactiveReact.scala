package slate
package views

import japgolly.scalajs.react.{CallbackTo, ReactComponentB, TopNode}
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable

import cats.Monoid

object ReactiveReact {
  case class ReactiveState[ST](reactivePart: ST, cancelableFuture: CancelableFuture[Unit])

  // todo: cancel on willUnmount
  implicit class ReactiveOps[P, ST, B, N <: TopNode](val builder: ReactComponentB[P, ST, B, N]) {

    @inline final def reactiveReplaceL[R]
    (getReactivePart: P => Observable[R], setReactivePart: (ST, R) => ST)
    (implicit sch: Scheduler): ReactComponentB[P, ST, B, N] =
      builder.componentWillMount($ =>
        CallbackTo.lift { () =>
          val _ = getReactivePart($.props).foreach { r =>
            val () = $.modState(setReactivePart(_, r)).runNow()
          }
        }
      )

    @inline final def reactiveReplace
    (implicit sch: Scheduler,
     propsAreReactiveEv: P =:= Observable[ST]
    ): ReactComponentB[P, ST, B, N] =
      reactiveReplaceL[ST](propsAreReactiveEv, (_, r) => r)

    @inline final def reactiveMonoid
    (implicit sch: Scheduler, ST: Monoid[ST],
     propsAreReactiveEv: P =:= Observable[ST]
    ): ReactComponentB[P, ST, B, N] =
      builder.componentWillMount($ =>
        CallbackTo.lift { () =>
          val _ = $.props.foldLeftF(ST.empty)(ST.combine).foreach { r =>
            val () = $.setState(r).runNow()
          }
        }
      )

  }

}
