package slate

trait Alg[T[_[_]], C[_[_]], A] {
  def apply[G[_] : C](tg: T[G]): G[A]
}

