package fpmax

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

  def checkContinue[F[_] : Program : Console](name: String): F[Boolean] =
    for {
      _      <- Console[F].putStrLn(s"Do you want to continue, $name?")
      answer <- Console[F].getStrLn
      result <- answer.toLowerCase() match {
        case "y" => Program[F].finish(true)
        case _ => Program[F].finish(false)
      }
    } yield result

  def gameLoop[F[_] : Program : Console : Random](name: String): F[Unit] =
    for {
      num   <- Random[F].nextInt(5)
      _     <- Console[F].putStrLn(s"$name, guess a number from 1 to 5:")
      input <- Console[F].getStrLn
      _     <- parseInt(input).fold(
        Console[F].putStrLn("Input is not a number."))(
        n => if (n == num) {
          Console[F].putStrLn(s"You guessed right, $name!")
        } else {
          Console[F].putStrLn(s"You guessed wrong, $name! The number was $num.")
        })
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else Program[F].finish(())
    } yield ()

  def main[F[_] : Program : Console : Random](): F[Unit] = {

    for {
      _    <- Console[F].putStrLn("What is your name?")
      name <- Console[F].getStrLn
      _    <- Console[F].putStrLn(s"Hello, $name, welcome to the game!")
      _    <- gameLoop(name)
    } yield ()
  }

  def mainIO(): IO[Unit] = main[IO]()
}
