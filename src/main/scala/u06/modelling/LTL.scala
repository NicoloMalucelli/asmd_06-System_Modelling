package scala.u06.modelling

import pc.examples.PNMutualExclusion.Place.{N, T}
import pc.modelling.PetriNet.*

import scala.annotation.{tailrec, targetName}
import pc.modelling.{PetriNet, System}

import scala.collection.immutable.Queue
import scala.u06.modelling.LTL.Condition

object LTL:

  trait Condition[P]:
    infix def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean

  extension [P](p: *[P])
    @targetName("less")
    infix def <(n: Integer): Condition[P] = (_, m) => m.asMap.getOrElse(p, 0) < n
    @targetName("greater")
    infix def >(n: Integer): Condition[P] = (_, m) => m.asMap.getOrElse(p, 0) > n
    infix def is(n: Integer): Condition[P] = (_, m) => m.asMap.getOrElse(p, 0) == n

  extension [P](p: Condition[P])
    infix def and(p2: Condition[P]): Condition[P] = (pNet, m) => p.eval(pNet, m) && p2.eval(pNet, m)
    infix def or(p2: Condition[P]): Condition[P] = (pNet, m) => p.eval(pNet, m) || p2.eval(pNet, m)
    infix def not: Condition[P] = (pNet, m) => !p.eval(pNet, m)
    def always: Condition[P] = new Condition[P]:
      override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = internalEval(pNet, Queue(m), Set())
      @tailrec
      private def internalEval(pNet: System[Marking[P]], toEval: Queue[Marking[P]], evaluated: Set[Marking[P]]): Boolean = toEval.dequeueOption match
          case None => true
          case Some((m, q)) if p.eval(pNet, m) =>
            internalEval(pNet, q enqueueAll (pNet.next(m) diff evaluated), evaluated + m)
          case _ => false

    def eventually: Condition[P] = new Condition[P]:
      override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = internalEval(pNet, m, Set())

      private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]]): Boolean = toEval match
        case m if p.eval(pNet, m) => true
        case m => pNet.next(toEval) match
          case mNext if mNext.diff(evaluated).isEmpty => false
          case mNext => mNext.diff(evaluated).map(newM => internalEval(pNet, newM, evaluated + m ++ mNext)).forall(identity)

    def next: Condition[P] = (pNet, m) => pNet.next(m).map(m => p.eval(pNet, m)).forall(identity)

    infix def until(q: Condition[P]): Condition[P] = new Condition[P]:
      override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = internalEval(pNet, m, Set(), false)

      private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]], pWasTrue: Boolean): Boolean = toEval match
        //case m if q.eval(pNet, m) => pWasTrue
        case m if !p.eval(pNet, m) => pWasTrue && q.eval(pNet, m)
        case m => pNet.next(toEval) match
          case mNext if mNext.diff(evaluated).isEmpty => false
          case mNext => mNext.diff(evaluated).map(newM => internalEval(pNet, newM, evaluated + m ++ mNext, true)).forall(identity)

    infix def weakUntil(q: Condition[P]): Condition[P] = new Condition[P]:
      override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = internalEval(pNet, m, Set())

      private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]]): Boolean = toEval match
        //case m if q.eval(pNet, m) => true
        //case m if !p.eval(pNet, m) => false
        case m if !p.eval(pNet, m) => q.eval(pNet, m)
        case m => pNet.next(toEval) match
          case mNext if mNext.diff(evaluated).isEmpty => true
          case mNext => mNext.diff(evaluated).map(newM => internalEval(pNet, newM, evaluated + m ++ mNext)).forall(identity)

import pc.examples.PNMutualExclusion.*
import LTL.*

@main def testLTL =
  //println(|(*(N)) and not(|(*(A))) eval MSet(*(N), *(B)))
  import PetriNet.*


  def pnME = PetriNet[Place](
    MSet(*(A)) ~~> MSet(*(B)),
    MSet(*(B)) ~~> MSet(*(C)),
    MSet(*(C)) ~~> MSet(*(D)),
    MSet(*(C)) ~~> MSet(*(B)),
  ).toSystem

  /*
  println(always(not(*(N))) eval (pnME, MSet(*(A)))) //true
  println(always(*(C)) eval (pnME, MSet(*(A)))) //false
  println()
  println(eventually(*(C)) eval (pnME, MSet(*(A)))) //true
  println(eventually(*(N)) eval (pnME, MSet(*(A)))) //false
  println()
  println(next(*(B)) eval (pnME, MSet(*(A)))) //true
  println(next(*(C)) eval (pnME, MSet(*(A)))) //false
  println()
  println((*(A) until *(B)) eval (pnME, MSet(*(A)))) //true
  println((*(A) until *(C)) eval (pnME, MSet(*(A)))) //false
  println((*(A) until *(C)) eval(PetriNet[Place](MSet(*(A)) ~~> MSet(*(A))).toSystem, MSet(*(A)))) //false
  println()
  println((*(A) weakUntil *(B)) eval(pnME, MSet(*(A)))) //true
  println((*(A) weakUntil *(C)) eval (pnME, MSet(*(A)))) //false
  println((*(A) weakUntil *(C)) eval(PetriNet[Place](MSet(*(A)) ~~> MSet(*(A))).toSystem, MSet(*(A)))) //true
  println()
  */

  def pnME2 = PetriNet[Place](
    MSet(*(A)) ~~> MSet(),
    MSet(*(A), *(B)) ~~> MSet(*(C)),
    MSet(*(C)) ~~> MSet(*(D)),
  ).toSystem

  //println(eventually(*(D)) eval(pnME2, MSet(*(A), *(B))))
  //println((*(B) is 0 weakUntil  *(D)) eval(pnME2, MSet(*(A), *(B))))
  println((*(B) is 1 weakUntil eventually(*(D))) eval(pnME2, MSet(*(A), *(B))))