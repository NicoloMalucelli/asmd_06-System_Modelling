package pc.modelling

import pc.utils.MSet

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.{HashMap, Map}

object PetriNet:
  // pre-conditions, effects, inhibition

  enum Color:
    case Black, Yellow, Red

  case class Trn[P](cond: Marking[P], eff: Marking[P], inh: Marking[P], priority: Int = 1)

  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[*[P]]

  import scala.u06.modelling.LTL.Condition
  @targetName("ColoredToken")
  case class *[P](p: P, color: Color = Color.Black) extends Condition[P]:
    infix override def eval(pNet: System[Marking[P]], m: Marking[P]): Boolean = m.contains(this)
    override def toString: String = color match
      case Color.Black => p.toString
      case c => c.toString + "_" + p


  // factory of A Petri Net
  def apply[P](transitions: Trn[P]*): PetriNet[P] = transitions.toSet

  // factory of a System, as a toSystem method
  extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      (for
        Trn(cond, eff, inh, priority) <- pn   // get any transition
        if m disjoined inh          // check inhibition
        out <- m extract cond       // remove precondition
      yield (priority, out union eff))
        .maxes(_._1)
        .map(_._2)          // add effect

    def toSystemWithCache: System[Marking[P]] = new System[Marking[P]]:
      val system: System[Marking[P]] = pn.toSystem
      var cache: Map[Marking[P], Set[Marking[P]]] = HashMap[Marking[P], Set[Marking[P]]]()

      @tailrec
      override def next(a: Marking[P]): Set[Marking[P]] = cache.get(a) match
        case Some(res) => res
        case None => 
          cache = cache + ((a, system.next(a))) 
          this.next(a) 

  extension [T](l: Set[T])
    def maxes[B: Ordering](f: T => B): Set[T] = l.maxByOption(f) match
        case None => Set.empty
        case Some(x) => l.filter(f(_) == f(x))

  // fancy syntax to create transition rules
  extension [P](self: Marking[P])
    def ~~> (y: Marking[P]) = Trn(self, y, MSet())
  extension [P](self: Trn[P])
    def ^^^ (z: Marking[P]) = self.copy(inh = z)
    def priority (p: Integer) = self.copy(priority = p)