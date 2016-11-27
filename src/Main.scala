import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    //println(Stream(1, 2, 3).map(_+3).append(Stream(100, 200)).map(_*2).flatmap(e => Stream(e, e*1000)).toList)
    //println(Stream.from(10).take(10).toList)
    println(Stream.fibonacci.take(10).toList)
  }

  def mergeLists[A](lists: List[A]*): List[A] = {
    lists.reverse.foldLeft(List.empty[A])((acc, list) => list ::: acc)
  }

  def listFlatMap[A, B](list: List[A])(fun: A => List[B]): List[B] = {
    list.foldLeft(List.empty[B])((acc, value) => acc ::: fun(value))
  }

  def hasSubsequence[A](list: List[A], seq: List[A]): Boolean = {
    if(list.length < seq.length)
      false
    else if(list.take(seq.length) == seq)
      true
    else hasSubsequence(list.tail, seq)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def go(prevprev: Int, prev: Int, count: Int): Int = count match {
      case 0 => prevprev + prev
      case _ => go(prev, prevprev + prev, count-1)
    }

    n match {
      case 0 => 1
      case 1 => 1
      case _ => go(1, 1, n-2)
    }
  }
}
