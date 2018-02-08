/** Higher-Order Functions */

def sum(f: Int => Int): (Int, Int) => Int =
  (a: Int, b: Int) =>
    if (a < b) f.apply(a) + sum(f)(a + 1, b) else 0

def sumSqrs = sum((a: Int) => a * a)

sumSqrs(1, 4)

/* The above is equivalent to

def sum(f: Function1[Int, Int]): Function2[Int, Int, Int] =
  new Anon2(f)

class Anon2(f: Function1[Int, Int]) extends Function2[Int, Int, Int] {
  def apply(a: Int, b: Int) = {
    if (a < b) f.apply(a) + sum(f).apply(a + 1, b) else 0
  }
}

class Anon1 extends Function1[Int, Int] {
  def apply(a: Int) = a * a
}

def sumSqrs = sum(new Anon1())
*/

/** Lists */

/*
sealed abstract class List {
  def ::(head: Int): List = new ::(head, this)
}

case object Nil extends List
case class ::(head: Int, tail: List) extends List
*/

val l1 = 1 :: Nil
val l2 = 2 :: 3 :: Nil
val l = 2 :: l1

val hd :: tl = l

l1 eq tl

def length[A](l: List[A]): Int = l match {
  case Nil => 0
  case hd :: tl => 1 + length(tl)
}

length(l)
length(Nil)

def foldRight[A, B](op: (A, B) => B)(z: B)(l: List[A]): B =
  l match {
    case Nil => z
    case hd :: tl => op(hd, foldRight(op)(z)(tl))
  }


def foldLeft[A, B](op: (B, A) => B)(z: B)(l: List[A]): B =
  l match {
    case Nil => z
    case hd :: tl => foldLeft(op)(op(z, hd))(tl)
  }

/*
def sum(l: List[Int]): Int = l match {
  case Nil => 0
  case hd :: tl => hd + sum(tl)
}
*/

def sum(l: List[Int]): Int =
  foldLeft((x: Int, y: Int) => x + y)(0)(l)

sum(l)

def map[A, B](f: A => B)(l: List[A]): List[B] =
  foldRight((hd: A, acc: List[B]) => f(hd) :: acc)(Nil)(l)

/*
def incr(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case hd :: tl => (hd + 1) :: incr(tl)
}
*/

def incr(l: List[Int]): List[Int] =
  map((x: Int) => x + 1)(l)

l
incr(l)

/*
def reverse[A](l: List[A]): List[A] = {
  def rev(l: List[A], acc: List[A]) = l match {
    case Nil => acc
    case hd :: tl => rev(l, hd :: acc)
  }
  rev(l, Nil)
}
 */

def reverse[A](l: List[A]): List[A] =
  foldLeft((acc: List[A], hd: A) => hd :: acc)(Nil)(l)

reverse(l)

// sum(l) using pre-defined foldLeft function
l.foldLeft(0)(_ + _)

// Dot product of two vectors of doubles
def dotProd(v1: List[Double], v2: List[Double]): Double =
  (v1, v2).zipped map (_ * _) sum

val v1 = List(3.0, 2.0, 1.0)
val v2 = List(1.0, 2.0, 3.0)

dotProd(v1, v2)

// For expressions

for {
  x <- l
} yield x + 1

// is equivalent to

l map (x => x + 1)

// Similarly

for {
  x <- l1
  y <- l2
} yield (x, y)

// is equivalent to

l1 flatMap (x => l2 map (y => (x, y)))

