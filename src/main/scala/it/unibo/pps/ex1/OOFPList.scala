package it.unibo.pps.ex1

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case ::(h, t) => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    this.foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)
  
  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = this.map[(A, B)](x => (x, value))

  def length(): Int = this.foldLeft[Int](0)((x, _) => x+1);
  //def indices(): List[Int] = this.foldLeft(0 :: Nil())()

  def indices(): List[Int] =
    def _indices1(list: List[A], acc: Int): List[Int] = list match {
      case h :: t => acc :: _indices1(t ,acc+1)
      case _ => Nil()
    }
    _indices1(this, 0)



  def indices2(): List[Int] =
    this.foldLeft((0, List[Int]()))((acc, _) => (acc._1 + 1, acc._2.append(acc._1 :: Nil())))._2

  def zipWithIndex2: List[(A, Int)] = ???
  //  this.foldRight((0, List[(A, Int)]()))(acc, _) => ()

  def zipWithIndex: List[(A, Int)] =
    def _zipWithIndex(list: List[A], index: Int): List[(A, Int)] = list match {
      case h :: t => (h, index) :: _zipWithIndex(t, index+1)
      case _ => Nil()
    }
    _zipWithIndex(this, 0)

  def partition(predicate: A => Boolean): (List[A], List[A]) = (this.filter(predicate), this.filter(x => !predicate(x)))
  def span(predicate: A => Boolean): (List[A], List[A]) = ???
  def takeRight(n: Int): List[A] = ???
  def collect(predicate: PartialFunction[A, A]): List[A] = ???
// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:
  import List.*
  val sas = 1 :: 2 :: 3 :: Nil()
  val reference = List(1, 2, 3, 4)
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.length()) // 4
  println(reference.indices()) // List(0, 1, 2, 3)
  println(reference.indices2()) // List(0, 1, 2, 3)
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)