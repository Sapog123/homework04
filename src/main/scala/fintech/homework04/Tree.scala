package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case leaf: Leaf[A] => f(leaf.value)
      case branch: Branch[A] => g(fold(branch.left)(f)(g), fold(branch.right)(f)(g))
    }
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = fold(t)((_) => 1)((l,r)=>l+r)

  def max(t: Tree[Int]): Int = fold(t)((v)=>v)((l,r)=> Math.max(l,r))

  def depth[A](t: Tree[A]): Int = fold(t)((_)=> 1)((l,r)=>Math.max(l,r)+1)

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v=> Leaf(f(v)):Tree[B])((l,r)=> Branch(l,r))
}
