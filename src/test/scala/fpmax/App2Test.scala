package fpmax

import fpmax.App2._
import fpmax.App2.ConsoleOut.{DoYouWantToContinue, GuessNumber, RightGuess, WelcomeToGame, WhatIsYourName}
import org.scalatest.FunSuite

case class TestData(input: Vector[String],
                    output: Vector[String],
                    nums: Vector[Int]) {

  // Helper function for debugging purposes.
  def showOutput: String = output.mkString(", ")
}

// This is essentially a state monad. We could use a library like `cats`
// instead of implementing it ourselves.
case class TestEffects[A](run: TestData => (TestData, A)) {
  def map[B](f: A => B): TestEffects[B] =
    TestEffects(td => run(td) match { case (t, a) => (t, f(a))})

  def flatMap[B](f: A => TestEffects[B]): TestEffects[B] =
    TestEffects(t1 => run(t1) match { case (t2, a) => f(a).run(t2) })

  def eval(t: TestData): TestData = run(t)._1
}

object TestEffects {
  def pure[A](a: A): TestEffects[A] = TestEffects((_, a))

  implicit val programInst: Program[TestEffects] =
    new Program[TestEffects] {
      def finish[A](a: => A): TestEffects[A] = TestEffects.pure(a)
      def chain[A, B](fa: TestEffects[A], afb: A => TestEffects[B]): TestEffects[B] = fa.flatMap(afb)
      def map[A, B](fa: TestEffects[A], ab: A => B): TestEffects[B] = fa.map(ab)
    }

  implicit val consoleInst: Console[TestEffects] =
    new Console[TestEffects] {
      def putStrLn(s: String): TestEffects[Unit] =
        TestEffects(t => (t.copy(output = t.output :+ s), ()))

      def getStrLn: TestEffects[String] =
        TestEffects(t => (t.copy(input = t.input.tail), t.input.head))
    }

  implicit val randomInst: Random[TestEffects] =
    new Random[TestEffects] {
      def nextInt(max: Int): TestEffects[Int] =
        TestEffects(t => (t.copy(nums = t.nums.tail), t.nums.head))
    }
}

class App2Test  extends FunSuite {

  def mainTestEffects(): TestEffects[Unit] = main[TestEffects]()

  val td1 = TestData(
    input = Vector("Igor", "1", "n"),
    output = Vector(),
    nums = Vector(1)
  )

  val res1 = Vector(
    WhatIsYourName.enc,
    WelcomeToGame("Igor").enc,
    GuessNumber("Igor").enc,
    RightGuess("Igor").enc,
    DoYouWantToContinue("Igor").enc
  )

  test("One try with a good guess") {
    assert(mainTestEffects().eval(td1).output === res1)
  }
}
