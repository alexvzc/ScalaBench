/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.mergesort

object MergeSortParallel extends MergeSortBase {
    import scala.actors.Future
    import scala.actors.Futures._

    private val MAX_ACTORS = Runtime.getRuntime.availableProcessors * 2;

    case class ArrayRegion(start : Int, length : Int)

    def futureReduce[B](futures : List[Future[B]], reduce_fn : (B, B) => B) : Future[B] = {

        def fReduce(futures : List[Future[B]]) : List[Future[B]] = {
            futures match {
                case Nil => Nil
                case f :: Nil => futures
                case f :: g :: tail => {
                        future { reduce_fn(f(), g()) } :: fReduce(tail)
                }
            }
        }

        futures match {
            case Nil => throw new IllegalArgumentException
            case f :: Nil => f
            case _ => futureReduce(fReduce(futures), reduce_fn)
        }
    }

    def mergeSort(array : Array[Int], start : Int, length : Int, scratch : Array[Int]) : Unit = {
        val size = (length + MAX_ACTORS - 1) / MAX_ACTORS;

        def regionSplit[B](start : Int, f : ArrayRegion => B) : List[B] = {
            if(start < length) {
                val next = start + size
                f(ArrayRegion(start, size min (length - start))) :: regionSplit(next, f)
            } else Nil
        }

        val futures = regionSplit(start,
            a => future { MergeSort.mergeSort(array, a.start, a.length, scratch); a } )

        val ff = futureReduce(futures, (a : ArrayRegion, b : ArrayRegion) => {
                mergeLists(array, a.start, a.length, b.length, scratch)
                ArrayRegion(a.start, a.length + b.length)
            })

        ff()
    }

}
