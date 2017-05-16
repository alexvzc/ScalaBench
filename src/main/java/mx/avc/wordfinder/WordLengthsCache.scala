/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.wordfinder

trait WordLengthsCache {
    import scala.collection.BitSet
    import scala.collection.mutable.SynchronizedMap
    import scala.collection.mutable.WeakHashMap

    private val cache = new WeakHashMap[Set[String], List[Int]]()
    with SynchronizedMap[Set[String], List[Int]]

    protected def dictionaryWordLengths(d : Set[String]) =
        cache.getOrElseUpdate(d, d.foldLeft(BitSet.empty)(_ + _.length).toList)

    @inline
    def addToSet[B, C <: B](a : Set[B], b : C) : Set[B] = {
        if(a contains b) a else a + b
    }

    @inline
    def mergeSets[B](a : Set[B], b : Set [B]) : Set[B] = {
        if(a.nonEmpty && b.nonEmpty) {
            val (c, d) = if (a.size > b.size) (a, b) else (b, a)
            if(d subsetOf c) c else c ++ d
        } else if(a nonEmpty) a else b
    }
}
