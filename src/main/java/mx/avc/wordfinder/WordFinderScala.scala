/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.wordfinder

object WordFinderScala extends WordLengthsCache {

    def findWords(cs : String, dictionary : Set[String]) : Set[Set[String]] = {
        val dictionary_word_lengths = dictionaryWordLengths(dictionary)

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

            if(incomplete.isEmpty)
                completed_paths
            else {
                @inline
                def recurse(w : String, p_paths : Set[Set[String]]) =
                    searchWords(sub_str.substring(w.length),
                                addToSet(path, w), p_paths)

                val x = incomplete.tail.foldLeft(completed_paths)((s, w) =>
                    mergeSets(s, recurse(w, Set.empty)))

                recurse(incomplete.head, x)
            }
        }

        searchWords(cs, Set.empty, Set.empty)
    }
}
