/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.wordfinder

object WordFinderParallel2 extends WordLengthsCache {
    import scala.actors.Actor
    import scala.actors.Actor._
    import scala.actors.threadpool.AtomicInteger

    private val MAX_ACTORS = Runtime.getRuntime.availableProcessors * 2

    @inline
    private def actorsToSpawn(partitions : Int, limit : Int,
                      running_actors : AtomicInteger) = {
        running_actors.synchronized {
            val r_actors = running_actors.get
            if(r_actors + partitions > limit) {
                running_actors.set(limit)
                limit - r_actors
            } else {
                running_actors.set(r_actors + partitions)
                partitions
            }
        }
    }

    @inline
    private def reduceWhile[B](initial : B)(cond : => Boolean)(work : => B)
            (implicit f: (B, B) => B) : B = {
        if(!cond) initial else reduceWhile(f(initial, work))(cond)(work)
    }

    def findWords(cs : String, dictionary : Set[String]) : Set[Set[String]] = {
        val dictionary_word_lengths = dictionaryWordLengths(dictionary)
        val actors_running = new AtomicInteger(1)
        val caller = self

        def searchWords(sub_str : String, path : Set[String], previous_paths : Set[Set[String]]) : Set[Set[String]] = {
            val subs_length = sub_str.length;
            val words_found = dictionary_word_lengths.
            foldLeft(Nil : List[String])((list, word_length) => {
                    if(word_length <= subs_length) {
                        val s = sub_str.substring(0, word_length)
                        if(dictionary contains s) s :: list else list
                    } else list
                })

            val (completed, incomplete) =
                words_found.foldLeft((Nil : List[String], Nil : List[String]))(
                    (l, w) => if(w.length == subs_length)
                        (w :: l._1, l._2) else (l._1, w :: l._2))

            val completed_paths = completed.foldLeft(previous_paths)(
                (l, w) => addToSet(l, addToSet(path, w)))

            if(incomplete.isEmpty) {
                completed_paths
            } else {
                @inline
                def recurse(w : String, p_paths : Set[Set[String]]) = 
                    searchWords(sub_str.substring(w.length), addToSet(path, w), p_paths)

                val actors_to_spawn = actorsToSpawn(
                    incomplete.size, MAX_ACTORS, actors_running)

                val remaining_branches = if(actors_to_spawn > 0) 
                    (1 to actors_to_spawn).foldLeft(incomplete)((l, i) => {
                            actor { caller ! recurse(l.head, Set.empty) }
                            l.tail
                        })
                else
                    incomplete

                if(remaining_branches.isEmpty) { 
                    completed_paths
                } else {
                    val x = remaining_branches.tail.foldLeft(completed_paths)(
                        (s, w) => mergeSets(s, recurse(w, Set.empty)))
                    
                    recurse(remaining_branches.head, x)
                }
            }
        }

        actor { caller ! searchWords(cs, Set.empty, Set.empty) }

        @inline implicit def doMergeSets[B](a : Set[B], b : Set[B]) : Set[B] =
                mergeSets(a, b)

        reduceWhile(Set.empty : Set[Set[String]])(actors_running.get > 0) {
            receive {
                case s : Set[Set[String]] => {
                        actors_running.decrementAndGet
                        s
                    }
            }
        }
    }
}
