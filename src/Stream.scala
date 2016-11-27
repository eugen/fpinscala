trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons[A](h(), t().take(n - 1))
    case _ => Stream.empty[A]
  }

  def drop(n: Int): Stream[A] = ???

  def toList: List[A] = {
    this match {
      case Empty => List.empty[A]
      case Cons(h, t) => {
        println(h())
        h() :: t().toList
      }
    }
  }

  def takeWhile(test: A => Boolean): Stream[A] = {
    this match {
      case Empty => Stream.empty
      case Cons(h, t) => if (test(h())) Stream.cons(h(), t().takeWhile(test)) else Stream.empty[A]
    }
  }

  def takeWhileWithFoldRight(test: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((e, next) => {
      if (test(e)) Stream.cons(e, next) else Stream.empty[A]
    })
  }

  def forAll(p: A => Boolean): Boolean = find(e => !p(e)).map(_ => true).getOrElse(false)

  def headOption: Option[A] = foldRight(None.asInstanceOf[Option[A]])((e, next) => Some(e))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B) = foldRight(Stream.empty[B])((e, next) => Stream.cons(f(e), next))

  def append[B >: A](e: Stream[B]) = foldRight(e)((e, next) => Stream.cons(e, next))

  def flatmap[B](f: A => Stream[B]) = foldRight(Stream.empty[B])((e, next) => f(e).append(next))

  def startsWith[B](s: Stream[B]): Boolean = ??? // combination of forAll(zipWith(), _==_) || drop(n)

  // TODO
  def scanRight = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n+1))

  def fibonacci: Stream[Int] = unfold((1, 1)) {
    case (n1, n2) => Some(n1, (n2, n1+n2))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, nextz)) => Stream.cons(a, unfold(nextz)(f))
    case _ => Stream.empty
  }

}


