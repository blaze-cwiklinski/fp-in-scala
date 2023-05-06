package training.ch02

import scala.annotation.tailrec

object ex01 {

    // Assuming that the "first" element is the fibonacci_bad(0).
    // Kept for reference.
    def fibonacci_bad(n: Int): Int = {
        def go(n: Int, acc: Int): Int = {
            if (n == 0 || n == 1) 1
            else go(n-1, acc+n-1) + go(n-2, acc+n-2)
        }
        if (n < 0) return -1
        else go(n, 0)
    }

    // Assuming that the "first" element is the fibonacci_optimal(0).
    def fibonacci_optimal(n: Int): Int = {
        @tailrec
        def go(x: Int, y: Int, z: Int): Int = {
            if (n == x) y
            else go(x+1, z, y+z)
        }
        if (n == 0 || n == 1) 1
        else go(0, 1, 1)
    }

    def main(args: Array[String]): Unit = {
        val string = "%d => %d"
        val interval = 0 to 5

        for (arg <- interval) println(string.format(arg, fibonacci_optimal(arg)))
    }
}