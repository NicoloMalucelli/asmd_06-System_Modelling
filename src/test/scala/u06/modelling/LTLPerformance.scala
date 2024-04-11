package scala.u06.modelling

import pc.modelling.PetriNet
import pc.modelling.PetriNet.*
import pc.utils.MSet

import scala.u06.modelling.LTL.{not, and, always}

object LTLPerformance:

  object Time:
    def timeOf(task: Runnable): Long =
      val start = System.nanoTime()
      task.run()
      val end = System.nanoTime()
      (end - start) / 1000000

  @main def performanceTest() =
    import Time.timeOf

    enum Place:
      case R_WAIT, READING, W_WAIT, WRITING, READY_TO_READ, READY_TO_WRITE, TOKEN

    import Place.*

    val pNet = PetriNet[Place](
      MSet(*(W_WAIT), *(TOKEN)) ~~> MSet(*(READY_TO_WRITE)),
      MSet(*(R_WAIT), *(TOKEN)) ~~> MSet(*(READY_TO_READ), *(TOKEN)),
      MSet(*(READY_TO_WRITE)) ~~> MSet(*(WRITING)) ^^^ MSet(*(READING)),
      MSet(*(READY_TO_READ)) ~~> MSet(*(READING)) ^^^ MSet(*(WRITING)),
      MSet(*(WRITING)) ~~> MSet(*(W_WAIT), *(TOKEN)),
      MSet(*(READING)) ~~> MSet(*(R_WAIT)),
    )

    val condition = always(
      not(*(WRITING) and *(READING)) and
      not(*(WRITING) > 1)
    )

    val initialMarkup = MSet.ofMap(Map((*(TOKEN), 1), (*(R_WAIT), 10), (*(W_WAIT), 3)))

    println("time without cache [ms]: " + timeOf(() => condition eval (pNet.toSystem, initialMarkup) ))
    println("time with cache [ms]:    " + timeOf(() => condition eval (pNet.toSystemWithCache, initialMarkup) ))