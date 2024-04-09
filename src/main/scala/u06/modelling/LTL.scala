package scala.u06.modelling

import pc.modelling.PetriNet.*

import scala.annotation.targetName

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

  extension [P](c1: Condition[P])
    def and(c2: Condition[P]): Condition[P] = And(c1, c2)
    def or(c2: Condition[P]): Condition[P] = Or(c1, c2)
    def not: Condition[P] = Not(c1)

import pc.examples.PNMutualExclusion.*
import LTL.*
import LTL.Condition.*

@main def testLTL =
  println(|(*(N)) and not(|(*(A))) eval MSet(*(N), *(B)))
