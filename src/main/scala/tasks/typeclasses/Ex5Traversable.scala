package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals
import u03.Optionals.Optional

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:
  trait Traversable[T[_]]:
    def traverseAll[A](ta: T[A])(cons: A => Unit) : Unit

  extension [A, T[_]](ta: T[A])
    private def logAll(using traversable: Traversable[T]): Unit =
      traversable.traverseAll(ta)(log)


  private def log[A](a: A): Unit = println("The next element is: "+a)


  given Traversable[Optional] with
    override def traverseAll[A](ta: Optional[A])(cons: A => Unit): Unit = ta match
      case Optional.Just(a) => cons(a)
      case Optional.Empty() => ()


  given Traversable[Sequence] with
    override def traverseAll[A](seq: Sequence[A])(cons: A => Unit): Unit = seq match
      case Cons(h, t) =>
        cons(h)
        traverseAll(t)(cons)
      case _ =>



  @main def tryTraversable =
    val seq = Cons(10, Cons(20, Cons(30, Nil())))
    val opt = Optional.Just(4)
    val none = Optional.Empty()

    logAll(seq)

    seq.logAll

    logAll(opt)

    opt.logAll
