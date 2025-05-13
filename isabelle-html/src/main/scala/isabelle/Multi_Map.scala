/*  Title:      Pure/General/multi_map.scala
    Author:     Makarius

Maps with multiple entries per key.
*/

package isabelle

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom


object Multi_Map
{
  def fromMap[K,V](m: Map[K, V]): Multi_Map[K, V] =
    new Multi_Map[K, V](m.view.mapValues(s => List(s)).toMap)

  private val empty_val: Multi_Map[Any, Nothing] = new Multi_Map[Any, Nothing](Map.empty)
  def empty[A, B] = empty_val.asInstanceOf[Multi_Map[A, B]]

  def apply[K,V](args: (K,V)*): Multi_Map[K, V] = {
    var res = new Multi_Map[K,V](Map())
    for ((k,v) <- args) {
      res = res + (k -> v)
    }
    res
  }

}


final class Multi_Map[A, +B] private(protected val rep: Map[A, List[B]])
  extends scala.collection.immutable.Map[A, B]
{
  /* Multi_Map operations */

  def iterator_list: Iterator[(A, List[B])] = rep.iterator

  def get_list(a: A): List[B] = rep.getOrElse(a, Nil)

  def insert[B1 >: B](a: A, b: B1): Multi_Map[A, B1] =
  {
    val bs = get_list(a)
    if (bs.contains(b)) this
    else new Multi_Map(rep + (a -> (b :: bs)))
  }

  def remove[B1 >: B](a: A, b: B1): Multi_Map[A, B1] =
  {
    val bs = get_list(a)
    if (bs.contains(b)) {
      bs.filterNot(_ == b) match {
        case Nil => new Multi_Map(rep - a)
        case bs1 => new Multi_Map(rep + (a -> bs1))
      }
    }
    else this
  }

  def ++[B1 >: B] (other: Multi_Map[A, B1]): Multi_Map[A, B1] =
    if (this eq other) this
    else if (isEmpty) other
    else
      (this.asInstanceOf[Multi_Map[A, B1]] /: other.rep.iterator) {
        case (m1, (a, bs)) => (bs :\ m1) { case (b, m2) => m2.insert(a, b) }
      }


  /* Map operations */

  override def stringPrefix = "Multi_Map"

  override def empty = Multi_Map.empty
  override def isEmpty: Boolean = rep.isEmpty

  override def keySet: Set[A] = rep.keySet

  override def iterator: Iterator[(A, B)] =
    for ((a, bs) <- rep.iterator; b <- bs.iterator) yield (a, b)

  def get(a: A): Option[B] = get_list(a).headOption

  override def + [B1 >: B](p: (A, B1)): Multi_Map[A, B1] = insert(p._1, p._2)

  override def ++ [B1 >: B](entries: GenTraversableOnce[(A, B1)]): Multi_Map[A, B1] =
    (this.asInstanceOf[Multi_Map[A, B1]] /: entries)(_ + _)

  override def removed (a: A): Multi_Map[A, B] =
    if (rep.isDefinedAt(a)) new Multi_Map(rep - a) else this

  override def updated[V1 >: B](key: A, value: V1): Map[A, V1] = insert(key, value)
}
