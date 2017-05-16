
package mx.avc.wordfinder;

import java.util.ArrayList;
import static java.util.Collections.EMPTY_SET;
import static java.util.Collections.unmodifiableList;
import static java.util.Collections.unmodifiableSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

public class WordFinder {

    private static final Map<Set<String>, List<Integer>> WORDLENGTH_CACHE =
            new WeakHashMap<Set<String>, List<Integer>>();

    private static List<Integer> dictionaryWordLengths(Set<String> dictionary) {
        synchronized(WORDLENGTH_CACHE) {
            if(WORDLENGTH_CACHE.containsKey(dictionary)) {
                return WORDLENGTH_CACHE.get(dictionary);
            }

            Set<Integer> word_lengths = new HashSet<Integer>();

            for(String word : dictionary) {
                word_lengths.add(word.length());
            }

            List<Integer> ret = unmodifiableList(
                    new ArrayList<Integer>(word_lengths));

            WORDLENGTH_CACHE.put(dictionary, ret);
            return ret;
        }
    }

    private static <T> Set<T> addToSet(Set<T> set, T elem) {
        if(set.contains(elem)) {
            return set;
        }

        Set<T> result = new HashSet<T>(set);
        result.add(elem);
        return unmodifiableSet(result);
    }

    private static Set<Set<String>> searchWords(String subc, Set<String> path,
            Set<String> dictionary, List<Integer> validWordLengths) {

        List<String> wordsFound = new ArrayList<String>();
        for(int i : validWordLengths) {
            if(i <= subc.length()) {
                String sstr = subc.substring(0, i);
                if(dictionary.contains(sstr)) {
                    wordsFound.add(sstr);
                }
            }
        }

        Set<Set<String>> results = new HashSet<Set<String>>();

        for(String word : wordsFound) {
            Set<String> new_path = addToSet(path, word);
            int wordLength = word.length();
            if(wordLength == subc.length()) {
                results.add(new_path);
            } else {
                results.addAll(searchWords(subc.substring(wordLength), new_path,
                        dictionary, validWordLengths));
            }
        }

        return unmodifiableSet(results);
    }

    public static Set<Set<String>> findWords(String cs,
            Set<String> dictionary) {
        return searchWords(cs, EMPTY_SET, dictionary,
                dictionaryWordLengths(dictionary));
    }

}
