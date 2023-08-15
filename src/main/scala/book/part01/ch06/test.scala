package fp.part01.ch06

import RNG._
import Simulation._

object test {
    
    def main(args: Array[String]): Unit = {
        val (res1, rng1) = SimpleRNG(13).nextInt
        println(res1)
        val (res2, rng2) = rng1.nextInt
        println(res2)
        val (res3, rng3) = rng2.nonNegativeInt
        println(res3)
        val (res4, rng4) = rng3.double
        println(res4)
        val ((res5, res6), rng5) = rng4.intDouble
        println(res5, res6)
        val ((res7, res8), rng6) = rng5.doubleInt
        println(res7, res8)
        val ((res9, res10, res11), rng7) = rng6.double3
        println(res9, res10, res11)
        val (l, r) = SimpleRNG(13).ints(5)
        println(l)
        val (res12, rng8) = doubleViaMap(rng7)
        println(res12)
        val (res13, rng9) = nonNegativeLessThan(13)(rng8)
        println(res13)
        val (res14, rng10) = nonNegativeLessThan(-13)(rng9)
        println(res14)
        val (res15, rng11) = mapViaFlatMap(int)(_ .toFloat)(rng10)
        println(res15)
        val (res16, rng12) = map2ViaFlatMap(int, int)(_.toString() + _.toString())(rng11)
        println(res16)
        val (res17, rng13) = dieRoll(rng12)
        println(res17)
        println()
        val m = Machine(true, 3, 0)
        println(m)
        // the first Turn is neglected, since the Machine is locked,
        // next input (a Coin) unlocks the Machine,
        // the second Turn will result in a next state modification,
        // since it was preceded with a Coin, so we're expecting
        // a transition from (true, 3, 0) to (true, 2, 1) state
        println(simulateMachine(List(Turn,Coin,Turn)).run(m))
    }
}