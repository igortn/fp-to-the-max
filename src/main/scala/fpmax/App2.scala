package fpmax

import fpmax.App2.ConsoleOut.{DoYouWantToContinue, GuessNumber, InputNotNumber, RightGuess, WelcomeToGame, WhatIsYourName, WrongGuess}

import scala.util.Try

object App2 {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  /*********************************************************************************/

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]
    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]
    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit pf: Program[F]): Program[F] = pf
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](ab: A => B)(implicit pf: Program[F]): F[B] = pf.map(fa, ab)
    def flatMap[B](afb: A => F[B])(implicit pf: Program[F]): F[B] = pf.chain(fa, afb)
  }

  /*********************************************************************************/

  trait Console[F[_]] {
    def putStrLn(s: String): F[Unit]
    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit cf: Console[F]): Console[F] = cf
  }

  /*********************************************************************************/

  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit rf: Random[F]): Random[F] = rf
  }

  /*********************************************************************************/

  class IO[A](val unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] =
      IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def apply[A](unsafeRun: () => A): IO[A] = new IO(unsafeRun)
    def pure[A](a: A): IO[A] = new IO(() => a)
  }

  /*********************************************************************************/

  object implicits {
    implicit val programInst: Program[IO] =
      new Program[IO] {
        def finish[A](a: => A): IO[A] = IO.pure(a)
        def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)
        def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
      }

    implicit val consoleInst: Console[IO] =
      new Console[IO] {
        def putStrLn(s: String): IO[Unit] = IO(() => println(s))
        def getStrLn: IO[String] = IO(() => scala.io.StdIn.readLine())
      }

    implicit val randomInst: Random[IO] =
      new Random[IO] {
        def nextInt(max: Int): IO[Int] = IO(() => scala.util.Random.nextInt(max) + 1)
      }
  }

  /*********************************************************************************/

  sealed trait ConsoleOut {
    def enc: String
  }

  object ConsoleOut {
    case object WhatIsYourName extends ConsoleOut {
      def enc = "What is your name?"
    }
    case class WelcomeToGame(name: String) extends ConsoleOut {
      def enc = s"Hello, $name, welcome to the game!"
    }
    case class GuessNumber(name: String) extends ConsoleOut {
      def enc = s"$name, guess a number from 1 to 5:"
    }
    case object InputNotNumber extends ConsoleOut {
      def enc = "Input is not a number."
    }
    case class RightGuess(name: String) extends ConsoleOut {
      def enc = s"You guessed right, $name!"
    }
    case class WrongGuess(name: String, num: Int) extends ConsoleOut {
      def enc = s"You guessed wrong, $name! The number was $num."
    }
    case class DoYouWantToContinue(name: String) extends ConsoleOut {
      def enc = s"Do you want to continue, $name?"
    }
  }

  /*********************************************************************************/

  def checkContinue[F[_] : Program : Console](name: String): F[Boolean] =
    for {
      _      <- Console[F].putStrLn(DoYouWantToContinue(name).enc)
      answer <- Console[F].getStrLn
      result <- answer.toLowerCase() match {
        case "y" => Program[F].finish(true)
        case _ => Program[F].finish(false)
      }
    } yield result

  def gameLoop[F[_] : Program : Console : Random](name: String): F[Unit] =
    for {
      num   <- Random[F].nextInt(5)
      _     <- Console[F].putStrLn(GuessNumber(name).enc)
      input <- Console[F].getStrLn
      _     <- parseInt(input).fold(
        Console[F].putStrLn(InputNotNumber.enc))(
        n => if (n == num) {
          Console[F].putStrLn(RightGuess(name).enc)
        } else {
          Console[F].putStrLn(WrongGuess(name, num).enc)
        })
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else Program[F].finish(())
    } yield ()

  def main[F[_] : Program : Console : Random](): F[Unit] = {

    for {
      _    <- Console[F].putStrLn(WhatIsYourName.enc)
      name <- Console[F].getStrLn
      _    <- Console[F].putStrLn(WelcomeToGame(name).enc)
      _    <- gameLoop(name)
    } yield ()
  }

  import implicits._

  def mainIO(): IO[Unit] = main[IO]()
}
