import scala.reflect.ClassTag

object PrefixSum {
  implicit class RichList[T: ClassTag](list: List[T]) {
    val n = list.size

    private
    implicit class PowerInt(a: Int) {
      import scala.math.pow
      def ** (b: Int): Int = pow(a, b).intValue
    }

    private
    def log2(x: Int) = {
      import scala.math.log
      val y = log(x) / log(2)
      y.ceil.toInt
    }

    def scan_(z: T)(f: (T, T) => T): List[T] = {
      val a = list.toArray.padTo(n+1, z)
      //def upsweep() =
      list
    }
  }
}

val list = List(2, 3, 6, 1, 8, 9)
val points = 1 to 100 par
val means = List(7, 21, 77) par
def distance(p1: Int, p2: Int) = if(p1 < p2) p2 - p1 else p1 - p2
val result = for {
  point <- points
  mean = means.minBy(distance(_, point))
} yield point -> mean
result.groupBy(_._2).mapValues(_.unzip._1)
result groupBy(_._2) mapValues(_.map(_._1))
for {
  (mean, cluster) <- result.groupBy(_._2)
  pointsByMean = cluster.map(_._1)
} yield mean -> pointsByMean

class Matrix(val x: Int)
val Matrix = 1
