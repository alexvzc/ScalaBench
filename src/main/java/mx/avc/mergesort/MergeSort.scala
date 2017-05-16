/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mx.avc.mergesort

object MergeSort extends MergeSortBase {

    override
    def mergeSort(array : Array[Int], start : Int, length : Int, scratch : Array[Int]) : Unit = {
        if(length < 2) array
        else {
            val length_a = length / 2
            val length_b = length - length_a
            mergeSort(array, start, length_a, scratch)
            mergeSort(array, start + length_a, length_b, scratch)
            mergeLists(array, start, length_a, length_b, scratch)
        }
    }
}
