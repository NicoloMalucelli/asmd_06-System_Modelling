package scala.u06.modelling

import pc.utils.MSet

object MyPetriNet{}
/*
  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

  abstract class FiringLogic[Trn[P]]:
    def canFire[P](m: Marking[P]): Boolean

  class BasicFiringLogic[Trn[P]] extends FiringLogic:
    override def canFire[P](m: Marking[P]): Boolean = true

  trait Trn[P]:
    val cond: MSet[P]
    val eff: MSet[P]

  
  object Trn:
    private case class TrnImpl[P](cond: MSet[P], eff: MSet[P])
*/