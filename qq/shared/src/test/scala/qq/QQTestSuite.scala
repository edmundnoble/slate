package qq

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.TimeUnit

import monix.execution.{Cancelable, Scheduler}
import monix.execution.schedulers.ExecutionModel
import org.scalatest.{AsyncFreeSpec, Matchers, OptionValues}

import scalaz.\/

abstract class QQTestSuite extends AsyncFreeSpec with Matchers with OptionValues {
  implicit def scheduler: Scheduler =
    new Scheduler {
      override def execute(runnable: Runnable): Unit = executionContext.execute(runnable)
      override def reportFailure(t: Throwable): Unit = executionContext.reportFailure(t)
      override def scheduleOnce(initialDelay: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        executionContext.execute(r)
        Cancelable.empty
      }
      override def scheduleWithFixedDelay(initialDelay: Long, delay: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        executionContext.execute(r)
        Cancelable.empty
      }
      override def scheduleAtFixedRate(initialDelay: Long, period: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        executionContext.execute(r)
        Cancelable.empty
      }
      override def currentTimeMillis(): Long = System.currentTimeMillis()
      override def executionModel: ExecutionModel = ExecutionModel.SynchronousExecution
    }
  implicit def convertDisjunctionToValuable[E, A](dis: E \/ A)(implicit pos: org.scalactic.source.Position): Valuable[A] =
    new Valuable(dis.toOption, pos)
}
