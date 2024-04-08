package pc.modelling

import pc.utils.MSet

object PetriNet:
  // pre-conditions, effects, inhibition
  
  case class Trn[P](cond: MSet[P], eff: MSet[P], inh: MSet[P], priority: Int = 1)

  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

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