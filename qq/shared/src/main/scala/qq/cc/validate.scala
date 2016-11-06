package qq
package cc

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import org.atnos.eff._
import Eff._
import cats.{Semigroup, Traverse}

// TODO: eliminated when runEitherCombine is in eff-cats
trait ValidatedInterpretation {

  import interpret.{Recurse, interpret1}

  final def by[E: Semigroup] = new partialAp[E]

  final class partialAp[E: Semigroup] {

    /**
      * Run an error effect.
      *
      * Accumulate errors in an applicative fashion.
      */
    final def runErrorParallel[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[E Validated ?, R, U]): Eff[U, E Validated A] = {
      val recurse = new Recurse[E Validated ?, U, E Validated A] {
        def apply[X](m: E Validated X): X Either Eff[U, E Validated A] =
          m match {
            case i@Validated.Invalid(e) =>
              EffMonad[U].pure(i: Validated[E, A]).right[X]

            case Validated.Valid(a) =>
              a.left
          }

        def applicative[X, T[_] : Traverse](ms: T[E Validated X]): T[X] Either (E Validated T[X]) =
          ms.sequence[E Validated ?, X].right
      }

      interpret1[R, U, E Validated ?, A, E Validated A](_.valid)(recurse)(r)
    }

  }

}

trait ValidatedCreation extends ValidatedTypes {
  def valid[R: _validated[E, ?], E, A](a: => A): Eff[R, A] =
    send[Validated[E, ?], R, A](a.valid)

  def invalid[R: _validated[E, ?], E, A](a: => E): Eff[R, A] =
    send[Validated[E, ?], R, A](a.invalid)
}

trait ValidatedTypes {
  type _Validated[E, R] = Validated[E, ?] <= R
  type _validated[E, R] = Validated[E, ?] |= R
  type _ValidatedNel[E, R] = ValidatedNel[E, ?] <= R
  type _validatedNel[E, R] = ValidatedNel[E, ?] |= R
}

object validated extends ValidatedCreation with ValidatedTypes with ValidatedInterpretation
