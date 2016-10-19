package qq.util

import scala.language.higherKinds

case class Fix[F[_]](unFix: F[Fix[F]])
