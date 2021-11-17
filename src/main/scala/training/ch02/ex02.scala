package training

import scala.annotation.tailrec

object ex02 {

    def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
        @tailrec
        def loop(index: Int): Boolean = {
            if (as.length < 2 || index + 1 == as.length) true
            else if (index + 1 < as.length && ordered(as(index), as(index + 1))) loop(index+1)
            else false
        }
        
        loop(0)
    }

    def main(args: Array[String]): Unit = {

        val ascendingList = 0 to 10 toList 
        val notAsendingList = 5 :: ascendingList
        val descendingList = ascendingList.reverse
        val notDescendingList = 5 :: descendingList

        // ASC logics: _ < _
        println(isSorted[Int](ascendingList, _ < _))
        println(isSorted[Int](notAsendingList, _ < _))
        // DESC logics: _ > _
        println(isSorted[Int](descendingList, _ > _))
        println(isSorted[Int](notDescendingList, _ > _))
    }
}