/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.mergesort;

import static java.lang.Runtime.getRuntime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import static java.util.concurrent.Executors.newFixedThreadPool;
import java.util.concurrent.Future;
import static mx.avc.mergesort.MergeSortJava.copyArrayRegion;

/**
 *
 * @author alexv
 */
public class MergeSortJavaParallel {

    private static class ArrayRegion {
        private final int start;
        private final int length;
        private ArrayRegion(int my_start, int my_end) {
            start = my_start;
            length = my_end;
        }
    }

    private static class Holder {
        private static final MergeSortJavaParallel INSTANCE =
                new MergeSortJavaParallel();
    }

    public static MergeSortJavaParallel getInstance() {
        return Holder.INSTANCE;
    }

    public static final int MAX_JOBS = getRuntime().availableProcessors() * 2;

    public final ExecutorService EXECUTOR = newFixedThreadPool(MAX_JOBS);

    public static int[] mergeSort(int[] array) {
        int[] result = new int[array.length];
        mergeSort(array, result, new int[array.length]);
        return result;
    }

    public static int[] mergeSort(int[] array, int[] my_array, int[] scratch) {
        copyArrayRegion(array, my_array, 0, array.length, 0);
        mergeSort(my_array, 0, my_array.length, scratch);
        return my_array;
    }

    public static void mergeSort(int[] array, int start, int length,
            int[] scratch) {
        ExecutorService executor = getInstance().EXECUTOR;
        List<Future<ArrayRegion>> futures = new ArrayList<>();
        int size = (length + MAX_JOBS - 1) / MAX_JOBS;

        int i = 0;
        do {
            int my_start = i;
            int my_size = size > (array.length - my_start) ?
                    (array.length - my_start) : size;
            futures.add(executor.submit(() -> {
                MergeSortJava.mergeSort(array, my_start, my_size, scratch);
                return new ArrayRegion(my_start, my_size);
            }));
            i = my_start + my_size;
        } while(i < length);

        while(futures.size() > 1) {
            List<Future<ArrayRegion>> new_futures = new ArrayList<>();
            for(int j = 0; j < futures.size(); j += 2) {
                if((j+1) < futures.size()) {
                    Future<ArrayRegion> f = futures.get(j);
                    Future<ArrayRegion> g = futures.get(j+1);

                    new_futures.add(executor.submit(() -> {
                        ArrayRegion a = f.get();
                        ArrayRegion b = g.get();
                        MergeSortJava.mergeLists(array, a.start, a.length,
                                b.length, scratch);
                        return new ArrayRegion(a.start, a.length + b.length);
                    }));
                } else {
                    new_futures.add(futures.get(j));
                }
            }
            futures = new_futures;
        }

        try {
            futures.get(0).get();
        } catch(InterruptedException | ExecutionException ie) {
            throw new RuntimeException(ie);
        }
    }
}
