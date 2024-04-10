package scala.u06.modelling

import pc.examples.PNMutualExclusion.Place.{N, T}
import pc.modelling.PetriNet.*

import scala.annotation.{tailrec, targetName}
import pc.modelling.{PetriNet, System}

import scala.collection.immutable.Queue

object LTL:

  trait Condition[P]:
    def eval(m: Marking[P]): Boolean
  object Condition:
    private case class BasicCondition[P](p: *[P]) extends Condition[P]:
      override def eval(m: Marking[P]): Boolean = m.contains(p)
    def apply[P](p: *[P]): Condition[P] = BasicCondition[P](p)

    @targetName("condition")
    def |[P](p: *[P]): Condition[P] = Condition[P](p)

  case class And[P](p1: Condition[P], p2: Condition[P]) extends Condition[P]:
    override def eval(m: Marking[P]): Boolean = p1.eval(m) && p2.eval(m)

  case class Or[P](p1: Condition[P], p2: Condition[P]) extends Condition[P]:
    override def eval(m: Marking[P]): Boolean = p1.eval(m) || p2.eval(m)

  case class Not[P](p: Condition[P]) extends Condition[P]:
    override def eval(m: Marking[P]): Boolean = !p.eval(m)

  trait Operator[P]:
    def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean

  case class Always[P](p: Condition[P]) extends Operator[P]:
    override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean =
      internalEval(pNet, Queue(m), Set())

    @tailrec
    private def internalEval(pNet: System[Marking[P]], toEval: Queue[Marking[P]], evaluated: Set[Marking[P]]): Boolean = toEval.dequeueOption match
      case None => true
      case Some((m, q)) if p.eval(m) =>
        internalEval(pNet, q enqueueAll (pNet.next(m) diff evaluated), evaluated + m)
      case _ => false

  case class Until[P](p: Condition[P], q: Condition[P]) extends Operator[P]:
    override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean =
      internalEval(pNet, m, Set(), false)

    private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]], pWasTrue: Boolean): Boolean = toEval match
      case m if q.eval(m) => pWasTrue
      case m if p.eval(m) =>
        val next = pNet.next(toEval)
        if next.diff(evaluated).isEmpty then false else
          next.diff(evaluated)
            .map(newM => internalEval(pNet, newM, evaluated + m ++ next, true)).forall(identity)
      case _ => false

  case class WeakUntil[P](p: Condition[P], q: Condition[P]) extends Operator[P]:
    override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean =
      internalEval(pNet, m, Set(), false)

    private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]], pWasTrue: Boolean): Boolean = toEval match
      case m if q.eval(m) => pWasTrue
      case m if p.eval(m) =>
        val next = pNet.next(toEval)
        next.diff(evaluated)
          .map(newM => internalEval(pNet, newM, evaluated + m ++ next, true)).forall(identity)
      case _ => false

  case class Next[P](p: Condition[P]) extends Operator[P]:
    override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = pNet.next(m).map(p.eval).forall(identity)

  case class Eventually[P](p: Condition[P]) extends Operator[P]:
    override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean =
      internalEval(pNet, m, Set())

    private def internalEval(pNet: System[Marking[P]], toEval: Marking[P], evaluated: Set[Marking[P]]): Boolean = toEval match
      case m if p.eval(m) => true
      case m =>
        val next = pNet.next(toEval)
        if next.diff(evaluated).isEmpty then false else
          next.diff(evaluated)
            .map(newM => internalEval(pNet, newM, evaluated + m ++ next)).forall(identity)

  extension [P](c1: Condition[P])
    def and(c2: Condition[P]): Condition[P] = And(c1, c2)
    def or(c2: Condition[P]): Condition[P] = Or(c1, c2)
    def not: Condition[P] = Not(c1)
    def always: Operator[P] = Always(c1)
    def until(c2: Condition[P]): Operator[P] = Until(c1, c2)
    def next: Operator[P] = Next(c1)
    def weakUntil(c2: Condition[P]): Operator[P] = WeakUntil(c1, c2)
    def eventually: Operator[P] = Eventually(c1)

import pc.examples.PNMutualExclusion.*
import LTL.*

import LTL.Condition.*
@main def testLTL =
  //println(|(*(N)) and not(|(*(A))) eval MSet(*(N), *(B)))
  import PetriNet.*


  def pnME = PetriNet[Place](
    MSet(*(N)) ~~> MSet(*(T)),
    MSet(*(T)) ~~> MSet(*(C)),
    MSet(*(C)) ~~> MSet()
  ).toSystem

  //println(Always(not(|(*(A)))) eval (pnME, MSet(*(N))))
  //println(always(|(*(N)) or |(*(T)) or |(*(C))) eval (pnME, MSet(*(N))))

  //println(|(*(N)) until |(*(T)) eval (pnME, MSet(*(N))))
  //println(|(*(N)) weakUntil |(*(C)) eval (pnME, MSet(*(N))))
  //println(next(|(*(T)) or |(*(C))) eval (pnME, MSet(*(N), *(T))))

  println(eventually(|(*(C))) eval (pnME, MSet(*(N))))