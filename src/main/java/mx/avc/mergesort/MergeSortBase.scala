/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.mergesort

trait MergeSortBase {

    @inline
    private def copyArrayRegion(source : Array[Int], dest : Array[Int], index : Int,
                length : Int, index_r : Int) : Int = {
        var my_index = index
        var my_index_r = index_r
        while(my_index < length) {
            dest(my_index_r) = source(my_index)
            my_index_r += 1
            my_index += 1
        }
        my_index_r
    }

    def mergeSort(array : Array[Int]) : Array[Int] = {
        mergeSort(array, new Array[Int](array.length),
                new Array[Int](array.length))
    }

    def mergeSort(array : Array[Int], my_array : Array[Int],
            scratch : Array[Int]) : Array[Int] = {
        copyArrayRegion(array, my_array, 0, array.length, 0)
        mergeSort(my_array, 0, my_array.length, scratch)
        my_array
    }

    def mergeSort(array : Array[Int], start : Int, length : Int,
            scratch : Array[Int]) : Unit

    def mergeLists(array : Array[Int], start_a : Int, length_a : Int,
                   length_b : Int, scratch : Array[Int]) : Unit = {
        val start_b = start_a + length_a
        val end_b = start_b + length_b

        copyArrayRegion(array, scratch, start_a, end_b, start_a)

        var my_index_a = start_a
        var my_index_b = start_b
        var my_index_r = start_a
        while(my_index_a < start_b && my_index_b < end_b) {
            val a_val = scratch(my_index_a)
            val b_val = scratch(my_index_b)
            if(a_val <= b_val) {
                array(my_index_r) = a_val
                my_index_a += 1
            } else {
                array(my_index_r) = b_val
                my_index_b += 1
            }
            my_index_r += 1
        }

        my_index_r = copyArrayRegion(scratch, array, my_index_a, start_b,
                my_index_r)
        copyArrayRegion(scratch, array, my_index_b, end_b, my_index_r)
    }

}
