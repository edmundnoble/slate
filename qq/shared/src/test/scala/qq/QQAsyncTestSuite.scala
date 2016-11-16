package qq

import java.util.concurrent.TimeUnit

import monix.execution.schedulers.ExecutionModel
import monix.execution.{Cancelable, Scheduler}
import org.scalatest.AsyncFreeSpec

import scala.concurrent.ExecutionContext

abstract class QQAsyncTestSuite extends AsyncFreeSpec with QQTestSuite with AsyncTestUtil {

  implicit val schedulerVal: Scheduler = scheduler(super.executionContext)

  def scheduler(implicit executionContext: ExecutionContext): Scheduler =
    new Scheduler {
      override def execute(runnable: Runnable): Unit = executionContext.execute(runnable)
      override def reportFailure(t: Throwable): Unit = executionContext.reportFailure(t)
      override def scheduleOnce(initialDelay: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        executionContext.execute(r)
        Cancelable.empty
      }
      override def scheduleWithFixedDelay(initialDelay: Long, delay: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        ???
      }
      override def scheduleAtFixedRate(initialDelay: Long, period: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        ???
      }
      override def currentTimeMillis(): Long = System.currentTimeMillis()
      override def executionModel: ExecutionModel = ExecutionModel.SynchronousExecution
      override def withExecutionModel(em: ExecutionModel): Scheduler = ???
    }

}
