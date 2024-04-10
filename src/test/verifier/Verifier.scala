package verifier

import pc.modelling.PetriNet
import pc.modelling.PetriNet.*
import pc.utils.MSet

import scala.u06.modelling.LTL.*
object Verifier:
  enum Place:
    case R_WAIT, READING, W_WAIT, WRITING, READY_TO_READ, READY_TO_WRITE, TOKEN

  import Place.*
  def readersAndWriters = PetriNet[Place](
    MSet(*(R_WAIT)) ~~> MSet(*(READING)) ^^^ MSet(*(WRITING)),
    MSet(*(W_WAIT)) ~~> MSet(*(WRITING)) ^^^ MSet(*(WRITING), *(READING)),
    MSet(*(WRITING)) ~~> MSet(*(W_WAIT)),
    MSet(*(READING)) ~~> MSet(*(R_WAIT))
  ).toSystem

  @main def testSafety =
    println(
      always(
        not(*(WRITING) and *(READING)) and
        not(*(WRITING) > 1)
      ) eval(readersAndWriters, MSet(*(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT))))

  /*
    liveness is not satisfied,
    indeed is not guaranteed that writers will ever write or readers will ever read.
    writers and readers may suffer starvation
  */
  @main def testLiveness =
    println((
      eventually( *(WRITING) is 1 ) and
      eventually( *(READING) > 0 )
    ) eval(readersAndWriters, MSet(*(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT))))

  def readersAndWritersWithLiveness = PetriNet[Place](
    MSet(*(W_WAIT), *(TOKEN)) ~~> MSet(*(READY_TO_WRITE)),
    MSet(*(R_WAIT), *(TOKEN)) ~~> MSet(*(READY_TO_READ), *(TOKEN)),
    MSet(*(READY_TO_WRITE)) ~~> MSet(*(WRITING)) ^^^ MSet(*(READING)),
    MSet(*(READY_TO_READ)) ~~> MSet(*(READING)) ^^^ MSet(*(WRITING)),
    MSet(*(WRITING)) ~~> MSet(*(W_WAIT), *(TOKEN)),
    MSet(*(READING)) ~~> MSet(*(R_WAIT)),
  ).toSystem

  @main def testSafetyV2 =
    println(
      always(
        not(*(WRITING) and *(READING)) and
        not(*(WRITING) > 1)
      ) eval(readersAndWritersWithLiveness, MSet(*(TOKEN), *(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT))))

  @main def testLivenessV2 =
    println(
      (
        ((*(TOKEN) is 1) weakUntil (*(TOKEN) is 0)) and
        ((*(READY_TO_WRITE) is 1) until (*(WRITING) is 0))
      ) eval(readersAndWritersWithLiveness, MSet(*(TOKEN), *(R_WAIT), *(R_WAIT), *(R_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT), *(W_WAIT))))

    // *(TOKEN) is 1 weakUntil *(TOKEN) is 0
    // ((*(TOKEN) is 1) weakUntil (eventually(*(WRITING) is 1))