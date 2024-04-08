package pc.examples

export pc.modelling.PetriNet
import pc.utils.MSet
import pc.modelling.PetriNet.Color.*

object PNMutualExclusion:

  enum Place:
    case N, T, A, B, C
    
  export Place.*
  export pc.modelling.PetriNet.*
  export pc.modelling.SystemAnalysis.*
  export pc.utils.MSet

  // DSL-like specification of a Petri Net
  def pnME = PetriNet[Place](
    MSet(*(N)) ~~> MSet(*(T)),
    MSet(*(T)) ~~> MSet(*(C)) ^^^ MSet(*(C)),
    MSet(*(C)) ~~> MSet()
  ).toSystem

  def pnMEWithPriority = PetriNet[Place](
    MSet(*(N)) ~~> MSet(*(T)) priority 2,
    MSet(*(T)) ~~> MSet(*(C)) ^^^ MSet(*(C)),
    MSet(*(C)) ~~> MSet()
  ).toSystem

  def pnMEWithColors = PetriNet[Place](
    MSet(*(A)) ~~> MSet(*(B)),
    MSet(*(B)) ~~> MSet(*(C, Red)),
    MSet(*(C, Red)) ~~> MSet(*(B, Red)),
    MSet(*(B, Red)) ~~> MSet(*(A)),
  ).toSystem

@main def mainPNMutualExclusion =
  import PNMutualExclusion.*
  // example usage
  println(pnME.paths(MSet(*(N),*(N)),7).toList.mkString("\n"))

@main def mainPNMutualExclusionWithPriority =
  import PNMutualExclusion.*
  // example usage
  println(pnMEWithPriority.paths(MSet(*(N), *(N)), 7).toList.mkString("\n"))

@main def mainPNWithColors =
  import PNMutualExclusion.*
  // example usage
  println(pnMEWithColors.paths(MSet(*(A), *(C, Color.Red)),3).toList.mkString("\n"))