/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.mergesort;

import static java.lang.Math.max;
import static java.lang.System.arraycopy;

/**
 *
 * @author alexv
 */
public class MergeSortJava {

    protected static int copyArrayRegion(int[] source, int[] dest, int index_a,
            int length, int index_r) {
        while(index_a < length) {
            dest[index_r++] = source[index_a++];
        }
        return index_r;
    }

    protected static void mergeLists(int[] array, int start_a, int length_a,
            int length_b, int[] scratch) {
        int start_b = start_a + length_a;
        int end_b = start_b + length_b;
        copyArrayRegion(array, scratch, start_a, end_b, start_a);

        int index_a = start_a;
        int index_b = start_b;
        int index_r = start_a;

        while(index_a < start_b && index_b < end_b) {
            int a = scratch[index_a];
            int b = scratch[index_b];
            if(a <= b) {
                array[index_r++] = a;
                index_a++;
            } else {
                array[index_r++] = b;
                index_b++;
            }
        }

        index_r = copyArrayRegion(scratch, array, index_a, start_b, index_r);
        copyArrayRegion(scratch, array, index_b, end_b, index_r);
    }

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
        if(length < 2) {
            return;
        }

        int length_a = length / 2;
        int length_b = length - length_a;
        mergeSort(array, start, length_a, scratch);
        mergeSort(array, start + length_a, length_b, scratch);
        mergeLists(array, start, length_a, length_b, scratch);
    }
}
