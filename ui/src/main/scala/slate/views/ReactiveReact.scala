package slate
package views

import cats.Monoid
import japgolly.scalajs.react.{CallbackTo, Children}
import japgolly.scalajs.react.component.ScalaBuilder
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable

object ReactiveReact {
  case class ReactiveState[ST](reactivePart: ST, cancelableFuture: CancelableFuture[Unit])

  // todo: cancel on willUnmount
  implicit class ReactiveOps[P, ST, B](val builder: ScalaBuilder.Step4[P, Children.None, ST, B]) {

    @inline final def reactiveReplaceL[R]
    (getReactivePart: P => Observable[R], setReactivePart: (ST, R) => ST)
    (implicit sch: Scheduler): ScalaBuilder.Step4[P, Children.None, ST, B] =
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
    ): ScalaBuilder.Step4[P, Children.None, ST, B] =
      reactiveReplaceL[ST](propsAreReactiveEv, (_, r) => r)

    @inline final def reactiveMonoio
    (implicit sch: Scheduler, ST: Monoid[ST],
     propsAreReactiveEv: P =:= Observable[ST]
    ): ScalaBuilder.Step4[P, Children.None, ST, B] =
      builder.componentWillMount($ =>
        CallbackTo.lift { () =>
          val _ = $.props.foldLeftF(ST.empty)(ST.combine).foreach { r =>
            val () = $.setState(r).runNow()
          }
        }
      )

  }

}
