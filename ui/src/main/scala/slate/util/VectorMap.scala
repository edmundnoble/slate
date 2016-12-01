package slate
package util

case class VectorMap[K, +V](values: Vector[(K, V)]) extends AnyVal {
  def get(key: K): Option[V] = values.find(_._1 == key).map(_._2)
  def indexOf(key: K): Option[Int] = values.zipWithIndex.find(_._1._1 == key).map(_._2)
  def updatedEnd[V1 >: V](t: (K, V1)): VectorMap[K, V1] =
    VectorMap[K, V1](indexOf(t._1) match {
      case None => values :+ t
      case Some(idx: Int) => values.updated(idx, t)
    })
  def updatedBegin[V1 >: V](t: (K, V1)): VectorMap[K, V1] =
    VectorMap[K, V1](indexOf(t._1) match {
      case None => t +: values
      case Some(idx: Int) => values.updated(idx, t)
    })
  def -(k: K): VectorMap[K, V] =
    VectorMap(indexOf(k).fold(values)(idx => values.take(idx) ++ values.drop(idx + 1)))
  def :+[V1 >: V](t: (K, V1)): VectorMap[K, V1] =
    updatedEnd(t)
  def +:[V1 >: V](t: (K, V1)): VectorMap[K, V1] =
    updatedBegin(t)
}
