package fp.part01.ch05

import Stream._

object test {

    def if2[A](condition: Boolean, onTrue: => A, onFalse: => A): A =
        if (condition) onTrue else onFalse

    def maybeTwice0(b: Boolean, i: => Int) = if (b) i+i else 0
    
    def maybeTwice1(b: Boolean, i: => Int) = {
        lazy val j = i
        if (b) j+j else 0
    }
    
    def main(args: Array[String]): Unit = {
        if2(13 > 10,
            onTrue  = println("hello"),
            onFalse = println("world")
        )
        println()
        maybeTwice0(true, {println("XD"); 42})
        maybeTwice1(true, {println("ABC"); 42})
        println()
        println(Stream(1,2,3).take(0).toList)
        println(Stream(1,2,3).take(4).toList)
        println()
        println(Stream(1,2,3).drop(0).toList)
        println(Stream(1,2,3).drop(4).toList)
        println()
        println(Stream(1,2,3).takeWhile(_ < 3).toList)
        println(Stream(1,2,3).takeWhile(_ => true).toList)
        println()
        println(Stream(1,2,3).exists(_ < 3))
        println(Stream(1,2,3).exists(_ => false))
        println()
        println(Stream(1,2,3).forAll(_ < 4))
        println(Stream(1,2,3).forAll(_ < 3))
        println()
        println(ones.take(5).toList)
        println(ones.exists(_ % 2 != 0))
        println(ones)
        //println(ones.map(_ + 1).exists(_ % 2 != 0)) // "stack trace is suppressed"
        println(ones.takeWhile( _ == 1))
        println(ones.forAll(_ != 1))
        println()
        println(constant("xxx").take(5).toList)
        println(from(5).take(5).toList)
        println(fibs.take(13).toList)
        println()
        println(fibsViaUnfold.take(13).toList)
        println(fromViaUnfold(5).take(5).toList)
        println(constantViaUnfold("xxx").take(5).toList)
        println(onesViaUnfold.take(5).toList)
        println()
        println(mapViaUnfold(Stream(1,2,3))(_ + 1).toList)
        println(takeViaUnfold(Stream(1,2,3))(5).toList)
        println(takeWhileViaUnfold(Stream(1,2,3))(_ < 3).toList)
        println(zipWithViaUnfold(Stream(1,2), Stream(1,2,3))((a, b) => a+b).toList)
        println(zipAllViaUnfold(Stream(1,2), Stream(1,2,3)).toList)
        println(startsWith(Stream(1,2,3), Stream(1,2)))
        println(tails(Stream(1,2,3)).toList.map(toList))
        println(scanRight(Stream(1,2,3))(0)(_ + _).toList)
    }
}