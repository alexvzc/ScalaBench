/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.mergesort;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

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
        private static final MergeSortJavaParallel instance = new MergeSortJavaParallel();
    }

    public static MergeSortJavaParallel getInstance() {
        return Holder.instance;
    }

    public ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 2);

    public static int[] mergeSort(int[] array) {
        int[] result = new int[array.length];
        mergeSort(array, result, new int[array.length]);
        return result;
    }

    public static int[] mergeSort(int[] array, int[] my_array, int[] scratch) {
        MergeSortJava.copyArrayRegion(array, my_array, 0, array.length, 0);
        mergeSort(my_array, 0, my_array.length, scratch);
        return my_array;
    }

    public static void mergeSort(final int[] array, final int start, final int length, final int[] scratch) {
        ExecutorService executor = getInstance().executor;
        List<Future<ArrayRegion>> futures = new ArrayList<Future<ArrayRegion>>();
        int maxJobs = Runtime.getRuntime().availableProcessors() * 2;
        int size = (length + maxJobs - 1) / maxJobs;

        int i = 0;
        do {
            final int my_start = i;
            final int my_size = size > (array.length - my_start) ? 
                    (array.length - my_start) : size;
            futures.add(executor.submit(new Callable<ArrayRegion>() {
                public ArrayRegion call() throws Exception {
                    MergeSortJava.mergeSort(array, my_start, my_size, scratch);
                    return new ArrayRegion(my_start, my_size);
                }
            }));
            i = my_start + my_size;
        } while(i < length);

        while(futures.size() > 1) {
            List<Future<ArrayRegion>> new_futures = new ArrayList<Future<ArrayRegion>>();
            for(int j = 0; j < futures.size(); j += 2) {
                if((j+1) < futures.size()) {
                    final Future<ArrayRegion> f = futures.get(j);
                    final Future<ArrayRegion> g = futures.get(j+1);

                    new_futures.add(executor.submit(new Callable<ArrayRegion>() {

                        public ArrayRegion call() throws Exception {
                            ArrayRegion a = f.get();
                            ArrayRegion b = g.get();
                            MergeSortJava.mergeLists(array, a.start, a.length,
                                    b.length, scratch);
                            return new ArrayRegion(a.start, a.length + b.length);
                        }
                    }));
                } else {
                    new_futures.add(futures.get(j));
                }
            }
            futures = new_futures;
        }

        try {
            futures.get(0).get();
        } catch(InterruptedException ie) {
            throw new RuntimeException(ie);
        } catch(ExecutionException ee) {
            throw new RuntimeException(ee);
        }
    }
}
