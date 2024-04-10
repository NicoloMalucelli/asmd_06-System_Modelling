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
        println(evaluated)
        internalEval(pNet, q enqueueAll (pNet.next(m) diff evaluated), evaluated + m)
      case _ => false

  extension [P](c1: Condition[P])
    def and(c2: Condition[P]): Condition[P] = And(c1, c2)
    def or(c2: Condition[P]): Condition[P] = Or(c1, c2)
    def not: Condition[P] = Not(c1)

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
  println(Always(|(*(N)) or |(*(T)) or |(*(C))) eval (pnME, MSet(*(N))))