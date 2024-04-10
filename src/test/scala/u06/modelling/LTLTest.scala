package scala.u06.modelling
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.shouldBe
import pc.examples.PNMutualExclusion.Place
import pc.examples.PNMutualExclusion.Place.{A, B, C, D, N}
import pc.modelling.PetriNet
import pc.modelling.PetriNet.*
import pc.modelling.{PetriNet, System}
import pc.utils.MSet

import LTL.*
class LTLTest extends AnyFunSuite:

  def pNet = PetriNet[Place](
    MSet(*(A)) ~~> MSet(*(B)),
    MSet(*(B)) ~~> MSet(*(C)),
    MSet(*(C)) ~~> MSet(*(D)),
    MSet(*(C)) ~~> MSet(*(B)),
  ).toSystem

  test("evaluating *(A) on a MultiSet containing *(A) must return true"):
    *(A) eval (pNet, MSet(*(A), *(B))) shouldBe true

  test("evaluating *(A) on a MultiSet not containing *(A) must return false"):
    *(A) eval (pNet, MSet(*(C), *(B))) shouldBe false

  test("MultiSet element cardinality: is"):
    *(A) is 0 eval (pNet, MSet(*(B), *(C), *(C))) shouldBe true
    *(A) is 1 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe false
    *(B) is 1 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe true
    *(C) is 2 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe true

  test("MultiSet element cardinality: >"):
    *(A) > 0 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe false
    *(B) > 0 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe true

  test("MultiSet element cardinality: <"):
    *(A) < 1 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe true
    *(B) < 1 eval(pNet, MSet(*(B), *(C), *(C))) shouldBe false

  test("And"):
    *(A) and *(B) eval(pNet, MSet(*(A), *(B))) shouldBe true
    *(C) and *(A) eval(pNet, MSet(*(A), *(B))) shouldBe false

  test("Or"):
    *(A) or *(B) eval(pNet, MSet(*(A), *(B))) shouldBe true
    *(C) or *(A) eval(pNet, MSet(*(A), *(B))) shouldBe true
    *(C) or *(D) eval(pNet, MSet(*(A), *(B))) shouldBe false

  test("Not"):
    not(*(A)) eval(pNet, MSet(*(A), *(B))) shouldBe false
    not(*(C)) eval(pNet, MSet(*(A), *(B))) shouldBe true

  test("operator next"):
    next(*(B)) eval(pNet, MSet(*(A))) shouldBe true
    next(*(A)) eval(pNet, MSet(*(A))) shouldBe false
    next(*(A)) eval(pNet, MSet(*(A), *(B))) shouldBe false

  test("operator always"):
    always(*(A)) eval(pNet, MSet(*(A))) shouldBe false
    always(*(A)) eval(PetriNet[Place](MSet(*(A)) ~~> MSet()).toSystem, MSet(*(A))) shouldBe false
    always(*(A)) eval(PetriNet[Place](MSet(*(A)) ~~> MSet(*(A))).toSystem, MSet(*(A))) shouldBe true

  test("operator eventually"):
    eventually(*(B)) eval(pNet, MSet(*(A))) shouldBe true
    eventually(*(N)) eval(pNet, MSet(*(A))) shouldBe false

    def pNet2 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(B)),
      MSet(*(A)) ~~> MSet(*(C))
    ).toSystem
    eventually(*(B)) eval(pNet2, MSet(*(A))) shouldBe false

  test("operator until"):
    def pNet2 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(B)),
    ).toSystem
    *(A) until *(B) eval(pNet2, MSet(*(A))) shouldBe true

    def pNet3 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(A)),
      MSet(*(A)) ~~> MSet(*(B)),
    ).toSystem
    *(A) until *(B) eval(pNet3, MSet(*(A))) shouldBe false

    def pNet4 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(A)),
      MSet(*(A)) ~~> MSet(*(C)),
      MSet(*(C)) ~~> MSet(*(A)),
      MSet(*(A)) ~~> MSet(*(B)),
    ).toSystem
    *(A) until *(B) eval(pNet4, MSet(*(A))) shouldBe false

  test("operator weakUntil"):

    def pNet2 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(B)),
    ).toSystem

    *(A) weakUntil *(B) eval(pNet2, MSet(*(A))) shouldBe true

    def pNet3 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(A)),
      MSet(*(A)) ~~> MSet(*(B)),
    ).toSystem

    *(A) weakUntil *(B) eval(pNet3, MSet(*(A))) shouldBe true

    def pNet4 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(A)),
      MSet(*(A)) ~~> MSet(*(C)),
      MSet(*(C)) ~~> MSet(*(A)),
      MSet(*(A)) ~~> MSet(*(B)),
    ).toSystem

    *(A) weakUntil *(B) eval(pNet4, MSet(*(A))) shouldBe false

  test("operator composition example: weakUntil and eventually"):

    def pNet2 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(),
      MSet(*(A), *(B)) ~~> MSet(*(C)),
      MSet(*(C)) ~~> MSet(*(D)),
    ).toSystem

    println((*(B) is 1 weakUntil eventually(*(D))) eval(pNet2, MSet(*(A), *(B))))


  test("operator composition example: eventually and until"):

    def pNet2 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(*(B)),
      MSet(*(B)) ~~> MSet(*(C)),
      MSet(*(C)) ~~> MSet(*(D)),
      MSet(*(D)) ~~> MSet(*(N)),
    ).toSystem

    (eventually(*(C)) until *(D)) eval(pNet2, MSet(*(A))) shouldBe true

    def pNet3 = PetriNet[Place](
      MSet(*(A)) ~~> MSet(),
      MSet(*(A)) ~~> MSet(*(B)),
      MSet(*(B)) ~~> MSet(*(C)),
      MSet(*(C)) ~~> MSet(*(D)),
      MSet(*(D)) ~~> MSet(*(N)),
    ).toSystem

    (eventually(*(C)) until *(D)) eval(pNet3, MSet(*(A))) shouldBe false
