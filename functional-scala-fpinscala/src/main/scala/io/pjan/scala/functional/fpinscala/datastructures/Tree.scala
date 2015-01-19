package io.pjan.scala.functional.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  // Exercise 3.25: implement `size` that counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Exercise 3.26: write `maximum`, a function that returns the maximum element in a tree of ints
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  // Exercise 3.27: implement `depth`, a function that returns the maximum path length from the root to any leaf
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  // Exercise 3.28: implement `map`, a function that modifies all the elements in a tree with a given function
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29: Generalize `size`, `maximum`, `depth`, and `map`, writing a new function fold that abstracts over their similarities.
  def fold[A, B](t: Tree[A])(fl: A => B)(fb: (B, B) => B): B = t match {
    case Leaf(a)      => fl(a)
    case Branch(l, r) => fb(fold(l)(fl)(fb), fold(r)(fl)(fb))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_.max(_))

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _.max(_))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Tree.leaf(f(a)))((b1, b2) => Tree.branch(b1, b2))

}
