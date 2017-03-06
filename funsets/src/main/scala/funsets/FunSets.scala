package funsets

import scala.annotation.tailrec

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean =
    s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singleElementSet(elem: Int): FunSet =
    // def f(x: Int) = { x == elem }
    // f
    x => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet =
    x => s(x) || t(x)
  // def f(x: Int): Boolean = { s(x) || t(x) }
  // f

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet =
    x => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet =
    x => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet =
    x => !(s(x) && !p(x))

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a.abs > bound) true
      else if (!p(a) && s(a)) false
      else iter(a + 1)
    }
    iter(-1000)
  }
  //TODO: forall : check if all integers verify the predicate

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a.abs > bound) false
      else if (p(a) && s(a)) true
      else iter(a + 1)
    }
    iter(-1000)
  }

  //TODO: check if at least an integer verifies the predicate

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = {

    x => s(f(x))

    /* def t(x:Int):Boolean={
      if (s(x))
        if(f(x)==)
        //x => f(x)
   else false
    }
    t*/
    /* def compose(s1: FunSet, f1: Int => Int)(x: Int): Boolean = {
      s1(f1(x))
    }

    compose(s, f)
 */
    //
    //retourne la fonction qui à x associe l'ensemble des x contenus dans
  }
  //TODO : transform into a new set

  def toSet(ints: List[Int]): FunSet =
    ??? //TODO : (optional) convert a list to Set

  def toList(set: FunSet): List[Int] =
    ??? //TODO : (optional) convert a Set to a List

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet) {
    println(toString(s))
  }
}
