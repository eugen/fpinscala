import scala.{None => _, Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case _ => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).map(u => xs.map(x => math.pow(x - u, 2)).sum)

  def varianceFlatmap(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(u => Some(xs.map(x => math.pow(x - u, 2)).sum))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Some(a.foldLeft(List.empty[A])((acc, o) => o.map(some => some :: acc).getOrElse(acc))).filter(_.nonEmpty)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    Some(as.foldLeft(List.empty[B])((acc, a) => f(a).map(b => b :: acc).getOrElse(acc))).filter(_.nonEmpty)

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}