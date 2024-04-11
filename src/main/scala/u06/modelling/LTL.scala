package scala.u06.modelling

import pc.examples.PNMutualExclusion.Place
import pc.examples.PNMutualExclusion.Place.{A, N, T, B, C, D}
import pc.modelling.PetriNet.*

import scala.annotation.{tailrec, targetName}
import pc.modelling.{PetriNet, System}
import pc.utils.MSet

import scala.collection.immutable.Queue

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
    def not: Condition[P] = (pNet, m) => !p.eval(pNet, m)
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
        case m if q.eval(pNet, m) => pWasTrue
        //case m if !p.eval(pNet, m) => pWasTrue && q.eval(pNet, m)
        case m => pNet.next(toEval) match
          case mNext if mNext.diff(evaluated).isEmpty => false
          case mNext => mNext.diff(evaluated).map(newM => internalEval(pNet, newM, evaluated + m ++ mNext, true)).forall(identity)

    infix def weakUntil(q: Condition[P]): Condition[P] = new Condition[P]:
      override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = internalEval(pNet, m, Set())

      private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]]): Boolean = toEval match
        case m if q.eval(pNet, m) => true
        case m if !p.eval(pNet, m) => false
        //case m if !p.eval(pNet, m) => q.eval(pNet, m)
        case m => pNet.next(toEval) match
          case mNext if mNext.diff(evaluated).isEmpty => true
          case mNext => mNext.diff(evaluated).map(newM => internalEval(pNet, newM, evaluated + m ++ mNext)).forall(identity)

@main def main() =
  val pNet = PetriNet[Place](
    MSet(*(N)) ~~> MSet(*(B)) ^^^ MSet(*(C)),
    MSet(*(C)) ~~> MSet(*(A)),
    MSet(*(B), *(A), *(A)) ~~> MSet(*(D)),
  ).toSystem

  println((*(A) weakUntil *(B)) eval(pNet, MSet(*(N), *(C), *(A))))