/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.scalabench

object App {
    import mx.avc.mergesort.MergeSort
    import mx.avc.mergesort.MergeSortJava
    import mx.avc.mergesort.MergeSortJavaParallel
    import mx.avc.mergesort.MergeSortParallel
    /*import mx.avc.wordfinder.WordFinder
    import mx.avc.wordfinder.WordFinderParallel
    import mx.avc.wordfinder.WordFinderParallel2
    import mx.avc.wordfinder.WordFinderScala*/
    import scala.actors.Scheduler
    import scala.util.Random

    /*val DICTIONARY = Set("a", "ab", "ba", "aba", "bab")

    val TEST_PATTERN = "ababababababbababaabbabababaababababaabbaba";*/

    val WARMUP_ITERATIONS = 100

    val ITERATIONS = 200

    def assertEquals[B](expected : B, actual : B) : Unit = {
        @inline
        def deepComp[T](e : Iterator[T], a : Iterator[T]) : Unit = {
            if(e.hasNext || a.hasNext) {
                if(!e.hasNext || !a.hasNext)
                    assert(false)

                assertEquals(e.next, a.next)
                deepComp(e, a)
            }
        }

        expected match {
            case e : Array[Int] => assert (java.util.Arrays.equals(e, actual.asInstanceOf[Array[Int]]))
            case s : Product => {
                    val b = s.productIterator
                    val a = actual.asInstanceOf[Product].productIterator
                    deepComp(b, a)
                }
            case _ => assert(expected == actual)
        }
    }

    @inline
    def benchmark[B](expected : B)(f : => B) = {
        val start = System.nanoTime
        val actual = f
        val time = System.nanoTime - start
        print(":")
        assertEquals(expected, actual)
        time
    }

    @inline
    def iterative_benchmark[B](expected : B)(iterations : Int)(f: => B) = {
        (((1 to iterations).foldLeft(0L)((average, _) =>
                    average + benchmark(expected) { f })) : Double) / iterations
    }

    @inline
    def batteryBench[B](msg : String)(expected : B)(f: => B) = {
        val iter_bench = iterative_benchmark(expected)(ITERATIONS)(f)
        println("\nAverage time for " + msg + " (ms): " + iter_bench / 1e6d)
    }

    @inline
    def generateArray(gen : => Int)(n : Int) : Array[Int] = {
        val array = new Array[Int](n)
        def populate(index : Int) : Unit = {
            if(index < n) {
                array(index) = gen
                populate(index + 1)
            }
        }
        populate(0)
        array
    }

    @inline def sortArray(array : Array[Int]) : Array[Int] = {
        val sorted = array.clone
        java.util.Arrays.sort(sorted)
        sorted
    }

    @inline
    implicit def scalaSetToJavaSet[A, B <% A](original : Set[B]) : java.util.Set[A] = {
        val java_set = new java.util.HashSet[A]
        original.foreach(java_set.add(_))
        java.util.Collections.unmodifiableSet(java_set)
    }

    @inline
    implicit def javaSetToScalaSet[A, B <% A](original : java.util.Set[B]) : Set[A] = {
        val i = original.iterator
        var scala_list : List[A] = Nil
        while(i.hasNext) scala_list = i.next :: scala_list
        scala_list.toSet
    }

    def printRTInfo = {
        import scala.collection.JavaConversions._
        val p : scala.collection.Map[String, String] = System.getProperties.asInstanceOf[java.util.Map[String, String]]
        @inline def pp(key : String) = println(key + ": " + p(key))
        pp("java.runtime.name")
        pp("java.runtime.version")
        pp("os.name")
        pp("os.version")
        pp("os.arch")
    }

    def main(args : Array[String]):Unit = {
        val random = Random
        /*val JAVA_DICTIONARY : java.util.Set[String] = DICTIONARY
        val JAVA_EXPECTED = WordFinder.findWords(TEST_PATTERN, JAVA_DICTIONARY)
        val EXPECTED : Set[Set[String]] = JAVA_EXPECTED*/
        val INITIAL_LIST = generateArray(random.nextInt)(1000000)
        val EXPECTED_LIST = sortArray(INITIAL_LIST)

        printRTInfo

        println("Warming up")

        val array_copy = new Array[Int](INITIAL_LIST.length);
        val scratch = new Array[Int](INITIAL_LIST.length);

        iterative_benchmark(/*JAVA_EXPECTED, EXPECTED, EXPECTED, EXPECTED,*/ EXPECTED_LIST, EXPECTED_LIST, EXPECTED_LIST, EXPECTED_LIST)(WARMUP_ITERATIONS) {
            (/*WordFinder.findWords(TEST_PATTERN, JAVA_DICTIONARY),
             WordFinderScala.findWords(TEST_PATTERN, DICTIONARY),
             WordFinderParallel2.findWords(TEST_PATTERN, DICTIONARY),
             WordFinderParallel.findWords(TEST_PATTERN, DICTIONARY),*/
             MergeSortJava.mergeSort(INITIAL_LIST, array_copy, scratch),
             MergeSortJavaParallel.mergeSort(INITIAL_LIST, array_copy, scratch),
             MergeSort.mergeSort(INITIAL_LIST, array_copy, scratch),
             MergeSortParallel.mergeSort(INITIAL_LIST, array_copy, scratch))
        }

        println(" complete!")
        println("Starting tests")
/*
        batteryBench("WordFinder Java non-parallelized")(JAVA_EXPECTED) {
            WordFinder.findWords(TEST_PATTERN, JAVA_DICTIONARY)
        }

        batteryBench("WordFinder Scala non-parallelized")(EXPECTED) {
            WordFinderScala.findWords(TEST_PATTERN, DICTIONARY)
        }

        batteryBench("WordFinder Scala parallelized-synced-var")(EXPECTED) {
            WordFinderParallel2.findWords(TEST_PATTERN, DICTIONARY)
        }

        batteryBench("WordFinder Scala parallelized-msg-only")(EXPECTED) {
            WordFinderParallel.findWords(TEST_PATTERN, DICTIONARY)
        }
*/
        batteryBench("MergeSort Java non-parallelized")(EXPECTED_LIST) {
            MergeSortJava.mergeSort(INITIAL_LIST, array_copy, scratch)
        }

        batteryBench("MergeSort Java parallelized-future-based")(EXPECTED_LIST) {
            MergeSortJavaParallel.mergeSort(INITIAL_LIST, array_copy, scratch)
        }

        batteryBench("MergeSort Scala non-parallelized")(EXPECTED_LIST) {
            MergeSort.mergeSort(INITIAL_LIST, array_copy, scratch)
        }

        batteryBench("MergeSort Scala parallelized-future-based")(EXPECTED_LIST) {
            MergeSortParallel.mergeSort(INITIAL_LIST, array_copy, scratch)
        }

        MergeSortJavaParallel.getInstance.executor.shutdown
        Scheduler.shutdown
    }
}
