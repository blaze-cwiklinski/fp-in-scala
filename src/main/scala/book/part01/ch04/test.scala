package fp.part01.ch04

import fp.part01.ch04.Option._
import fp.part01.ch04.Either.{mean => meanE, traverse => traverseE, sequence => sequenceE}
import fp.part01.ch04.Person._

object test {

    def main(args: Array[String]): Unit = {
        val l = List(3, 4, 4, 6, 8.0)
        println("list: " + l)
        println("mean: " + mean(l))
        println("variance: " + variance(l))

        println(absOpt(Some(-10.2)))
        println(map2(Some("222"), Some("111"))((x: String, y: String) => Some(x.toInt + y.toInt)))
        
        val listOpt = List(Some(1), Some(2))
        println(listOpt)
        println(sequence(listOpt))
        
        println(None.getOrElse("XD"))

        println(Some(1).map(_ * 10))
        println(Some(2).flatMap(Some(_)))
        println(Some(3).getOrElse("xxx"))
        println(Some(4).orElse(Some(31)))
        println(Some(5).filter(_ > 1000))
        println(Some(6).filter(_ < 1000))

        println("list: " + l)
        println("mean: " + meanE(l))

        println(traverseE(l)(e => Right(e)))
        println(mkPerson("johhny", 13))
        println(mkPerson("", 13))
        println(mkPerson("johhny", -1))

    }
}
