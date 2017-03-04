package qq

import java.util.concurrent._

import qq.util.Recursion
import qq.util.Recursion.RecursionEngine
import qq.util.Unsafe.{GenericBuilderFactory, Liskov1}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.WrappedArray

object Platform {

  object Rec {
    implicit val defaultRecScheme: RecursionEngine =
      Recursion.Unsafe.LimitStack(maxStackSize = 50)
  }

  object Js {
    implicit val sexs = new ScheduledExecutorService {
      override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long, unit: TimeUnit): ScheduledFuture[_] = {
        val _ = js.timers.setTimeout(TimeUnit.MILLISECONDS.convert(initialDelay, unit)) {
          command.run()
          val _ = js.timers.setInterval(TimeUnit.MILLISECONDS.convert(period, unit))(command.run())
        }
        new ScheduledFuture[Unit] {
          override def getDelay(unit: TimeUnit): Long = unit.convert(initialDelay, unit)

          override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

          override def isCancelled: Boolean = ???

          override def isDone: Boolean = ???

          override def get(): Unit = ???

          override def get(timeout: Long, unit: TimeUnit): Unit = ???

          override def compareTo(o: Delayed): Int = initialDelay.compareTo(o.getDelay(unit))
        }
      }

      override def scheduleWithFixedDelay(command: Runnable, initialDelay: Long, delay: Long, unit: TimeUnit): ScheduledFuture[_] = {
        val _ = js.timers.setTimeout(TimeUnit.MILLISECONDS.convert(initialDelay, unit)) {
          command.run()
          val _ = js.timers.setInterval(TimeUnit.MILLISECONDS.convert(delay, unit))(command.run())
        }
        new ScheduledFuture[Unit] {
          override def getDelay(unit: TimeUnit): Long = unit.convert(initialDelay, unit)

          override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

          override def isCancelled: Boolean = ???

          override def isDone: Boolean = ???

          override def get(): Unit = ???

          override def get(timeout: Long, unit: TimeUnit): Unit = ???

          override def compareTo(o: Delayed): Int = delay.compareTo(o.getDelay(unit))
        }
      }

      override def schedule(command: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture[_] = {
        val _ = js.timers.setTimeout(TimeUnit.MILLISECONDS.convert(delay, unit))(command.run())
        new ScheduledFuture[Unit] {
          override def getDelay(otherUnit: TimeUnit): Long = otherUnit.convert(delay, unit)

          override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

          override def isCancelled: Boolean = false

          override def isDone: Boolean = false

          override def get(): Unit = ???

          override def get(timeout: Long, unit: TimeUnit): Unit = ???

          override def compareTo(o: Delayed): Int = delay.compareTo(o.getDelay(unit))
        }
      }

      override def schedule[V](callable: Callable[V], delay: Long, unit: TimeUnit): ScheduledFuture[V] = {
        val _ = js.timers.setTimeout(TimeUnit.MILLISECONDS.convert(delay, unit)) {
          val _ = callable.call()
        }
        new ScheduledFuture[V] {
          override def getDelay(unit: TimeUnit): Long = ???

          override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

          override def isCancelled: Boolean = ???

          override def isDone: Boolean = ???

          override def get(): V = ???

          override def get(timeout: Long, unit: TimeUnit): V = ???

          override def compareTo(o: Delayed): Int = delay.compareTo(o.getDelay(unit))
        }
      }

      override def submit[T](task: Callable[T]): Future[T] = {
        val _ = js.timers.setTimeout(0) {
          val _ = task.call()
        }
        new Future[T] {
          override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

          override def isCancelled: Boolean = false

          override def isDone: Boolean = false

          override def get(): T = ???

          override def get(timeout: Long, unit: TimeUnit): T = ???
        }
      }

      override def submit[T](task: Runnable, result: T): Future[T] = {
        val _ = js.timers.setTimeout(0)(task.run())
        new Future[T] {
          override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

          override def isCancelled: Boolean = ???

          override def isDone: Boolean = ???

          override def get(): T = ???

          override def get(timeout: Long, unit: TimeUnit): T = ???
        }
      }

      override def submit(task: Runnable): Future[_] = {
        val _ = js.timers.setTimeout(0)(task.run())
        new Future[Unit] {
          override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

          override def isCancelled: Boolean = ???

          override def isDone: Boolean = ???

          override def get(): Unit = ???

          override def get(timeout: Long, unit: TimeUnit): Unit = ???
        }
      }

      override def isTerminated: Boolean = false

      override def invokeAll[T](tasks: java.util.Collection[_ <: Callable[T]]): java.util.List[Future[T]] = ???

      override def invokeAll[T](tasks: java.util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): java.util.List[Future[T]] = ???

      override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = false

      override def shutdownNow(): java.util.List[Runnable] = new java.util.ArrayList()

      override def invokeAny[T](tasks: java.util.Collection[_ <: Callable[T]]): T = ???

      override def invokeAny[T](tasks: java.util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T = ???

      override def shutdown(): Unit = ()

      override def isShutdown: Boolean = false

      override def execute(command: Runnable): Unit = {
        val _ = js.timers.setTimeout(0)(command.run())
      }
    }

    implicit class objectOps(val obj: js.Object) extends AnyVal {
      def toDictionary: js.Dictionary[Any] = obj.asInstanceOf[js.Dictionary[Any]]
    }

    object Unsafe {
      implicit val jsWrappedArray: GenericBuilderFactory[js.WrappedArray] = new GenericBuilderFactory[js.WrappedArray] {
        override def newBuilder[A]: mutable.Builder[A, WrappedArray[A]] = js.WrappedArray.newBuilder[A]
      }
      implicit val jsWrappedArrayLiskovSeq: Liskov1[js.WrappedArray, Iterable] = new Liskov1[js.WrappedArray, Iterable] {
        override def apply[A]: js.WrappedArray[A] <:< Iterable[A] = implicitly
      }
    }

  }

}
