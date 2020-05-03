package redbook.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new UnsupportedOperationException("tail of empty list")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new UnsupportedOperationException("set head of empty list")
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new UnsupportedOperationException("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumWithFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productWithFoldLeft(ns: List[Int]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthWithFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)((acc, curr) => acc + 1)

  def reverse[A](ns: List[A]) =
    foldLeft(ns, List[A]())((acc, curr) => Cons(curr, acc))

  def reverseInTermsOfFoldRight[A](ns: List[A]) =
    foldRight(ns, List[A]())((curr, acc) => append(acc, Cons(curr, Nil)))

  def foldLeftInTermsOfFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverseInTermsOfFoldRight(l), z)((curr, acc) => f(acc, curr))

  def foldRightInTermsOfFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b, a))

  def appendInTermsOfFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def appendInTermsOfFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(List.reverse(a1), a2)((acc, curr) => Cons(curr, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((curr, acc) => Cons(curr + 1, acc))

  def stringifyDouble(l: List[Double]): List[String] =
    foldRight(l, List[String]())((curr, acc) => Cons(curr.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((curr, acc) => Cons(f(curr), acc))

  // a version using mutable state copied from answer book out of interest
  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, List[A]())((curr, acc) => if (f(curr)) Cons(curr, acc) else acc)

  def flatMapWithFoldRight[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, List[B]())((curr, acc) => append(f(curr), acc))

  def flatMapWithConcat[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    flatMapWithConcat(l)(f)

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(curr => if (f(curr)) List(curr) else Nil)

  def zipAndAdd(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAndAdd(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // My version of hasSubsequence. One bug is that apparently an empty set is a subset of itself, maybe there are more.
  @tailrec
  def listsMatch[A](a: List[A], b: List[A]): Boolean = (a, b) match {
    case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && listsMatch(t1, t2)
    case _ => true
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case Cons(h1, t1) => listsMatch(sup, sub) || hasSubsequence(t1, sub)
    }
  }

  // has subsequence as implemented in the answer book
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def answerBookHasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => answerBookHasSubsequence(t, sub)
  }
}

object TestHasSubsequence {
  import List._

  def main(args: Array[String]): Unit = {
    println(s"""
               |${hasSubsequence(List(), List())},
               |${hasSubsequence(List(1), List())},
               |${hasSubsequence(List(1), List(1))},
               |${hasSubsequence(List(1,2,3), List(1))},
               |${hasSubsequence(List(1,2,3), List(2))},
               |${hasSubsequence(List(1,2,3), List(3))},
               |${hasSubsequence(List(1,2,3), List(1,2))},
               |${hasSubsequence(List(1,2,3), List(2,3))},
               |${hasSubsequence(List(1,2,3), List(1,2,3))},
               |${hasSubsequence(List(1,2,3), List(4))}, expected false,
               |${hasSubsequence(List(1,2,3), List(1,4))}, expected false""".stripMargin)

    println(s"""
               |${answerBookHasSubsequence(List(), List())},
               |${answerBookHasSubsequence(List(1), List())},
               |${answerBookHasSubsequence(List(1), List(1))},
               |${answerBookHasSubsequence(List(1,2,3), List(1))},
               |${answerBookHasSubsequence(List(1,2,3), List(2))},
               |${answerBookHasSubsequence(List(1,2,3), List(3))},
               |${answerBookHasSubsequence(List(1,2,3), List(1,2))},
               |${answerBookHasSubsequence(List(1,2,3), List(2,3))},
               |${answerBookHasSubsequence(List(1,2,3), List(1,2,3))},
               |${answerBookHasSubsequence(List(1,2,3), List(4))}, expected false,
               |${answerBookHasSubsequence(List(1,2,3), List(1,4))}, expected false""".stripMargin)
  }
}

object ExerciseOnPatternMatching {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  // matches the 3rd one
}