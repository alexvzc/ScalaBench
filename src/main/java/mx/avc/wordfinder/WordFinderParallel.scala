/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.wordfinder

object WordFinderParallel extends WordLengthsCache {
    import scala.actors.Actor
    import scala.actors.Actor._
    import scala.actors.TIMEOUT

    private val MAX_ACTORS = Runtime.getRuntime.availableProcessors * 2

    private type L = List[String]
    private type S = Set[String]
    private type R = Set[S]

    case class PartialResultMsg(from : Actor, result : R)
    case class SpawnedActorsMsg(from : Actor, spawnedActors : List[Actor])

    @inline
    private def reduceWhile[B](initial : B)(cond : B => Boolean)(work : B => B) : B = {
        if(!cond(initial)) initial else reduceWhile(work(initial))(cond)(work)
    }

    @inline def my_actor(f : => R)(implicit caller : Actor) =
        new Actor { def act = {caller ! PartialResultMsg(self, f) } }

    def findWords(cs : String, dictionary : S) : R = {
        val dictionary_word_lengths = dictionaryWordLengths(dictionary)
        implicit val caller = self

        def searchWords(sub_str : String, path : S, previous_paths : R) : R = {
            val subs_length = sub_str.length;
            val words_found = dictionary_word_lengths.
            foldLeft(Nil : L)((list, word_length) => {
                    if(word_length <= subs_length) {
                        val s = sub_str.substring(0, word_length)
                        if(dictionary contains s) s :: list else list
                    } else list
                })

            val (completed, incomplete) =
                words_found.foldLeft((Nil : L, Nil : L))(
                    (l, w) => if(w.length == subs_length)
                        (w :: l._1, l._2) else (l._1, w :: l._2))

            val completed_paths = completed.foldLeft(previous_paths)(
                (l, w) => addToSet(l, addToSet(path, w)))

            if(incomplete.isEmpty) {
                completed_paths
            } else {
                @inline
                def recurse(w : String, p_paths : R) = 
                    searchWords(sub_str.substring(w.length), addToSet(path, w), p_paths)

                val actors_to_spawn = receiveWithin(0) {
                    case num_actors : Int => num_actors
                    case TIMEOUT => 0
                } min (incomplete.size - 1)

                val remaining_branches = if(actors_to_spawn > 0) {
                    val r = (1 to actors_to_spawn).foldLeft(
                        (Nil : List[Actor], incomplete))((l, i) => {
                            val new_actor = 
                                my_actor{ recurse(l._2.head, Set.empty) }
                            (new_actor :: l._1, l._2.tail)
                        })
                    caller ! SpawnedActorsMsg(self, r._1)
                    r._1.foreach(a => a.start)
                    r._2
                } else
                    incomplete

                if(remaining_branches.isEmpty) { 
                    completed_paths
                } else {
                    val x = remaining_branches.tail.foldLeft(completed_paths)(
                        (s, w) => mergeSets(s, recurse(w, Set.empty) ))

                    recurse(remaining_branches.head, x)
                }
            }
        }

        var initial_actor = my_actor { searchWords(cs, Set.empty, Set.empty) }
        initial_actor ! (MAX_ACTORS - 1)
        initial_actor.start

        case class AccR(result : Set[Set[String]], msgSent : Boolean, actors : List[Actor])

        reduceWhile(AccR(Set.empty, false, initial_actor :: Nil))(_.actors.nonEmpty)(
            (acc) => {
                receive {
                    case msg : PartialResultMsg => {
                            val new_actors = acc.actors.filterNot(_ == msg.from)
                            val new_msgSent = if(new_actors.nonEmpty && (acc.actors.head == msg.from || !acc.msgSent)) {
                                new_actors.head ! (MAX_ACTORS - new_actors.size)
                                true
                            } else {
                                false
                            }
                            AccR(mergeSets(acc.result, msg.result),
                                 new_msgSent, new_actors)
                    }
                    case msg : SpawnedActorsMsg =>
                        AccR(acc.result, false, acc.actors ++ msg.spawnedActors)
                }
            }
        ).result
    }
}
