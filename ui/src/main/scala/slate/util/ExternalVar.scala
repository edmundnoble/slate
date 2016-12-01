package slate.util

final case class ExternalVar[A](getter: () => A, setter: A => Unit) {
  def imap[B](to: A => B)(from: B => A): ExternalVar[B] =
    ExternalVar[B](() => to(getter()), b => setter(from(b)))
}
