package qq

import monix.eval.Task

import scalaz.{Monad, ~>}

object Util {

  implicit object TaskMonad extends Monad[Task] {
    override def point[A](a: => A): Task[A] = Task.Now(a)
    override def bind[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = fa.flatMap(f)
  }

  def withPrefixes[A](xss: List[List[A]], ys: List[A]): List[List[A]] =
    for {xs <- xss; y <- ys} yield y :: xs

  def foldWithPrefixes[A](firstPrefix: List[A], nextPrefices: List[A]*): List[List[A]] =
    nextPrefices.foldLeft(firstPrefix :: Nil)(withPrefixes)

  def single: Option ~> Seq = new (Option ~> Seq) {
    def apply[A](op: Option[A]): Seq[A] =
      op.fold(Vector.empty[A])(Vector.empty[A] :+ _)
  }

}
