package fpmax

import scala.util.Try

object App1 {

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

  def getStrLn: IO[String] =
    IO(() => scala.io.StdIn.readLine())

  def putStrLn(s: String): IO[Unit] =
    IO(() => println(s))

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def nextInt(max: Int): IO[Int] =
    IO(() => scala.util.Random.nextInt(max) + 1)

  def checkContinue(name: String): IO[Boolean] =
    for {
      _      <- putStrLn(s"Do you want to continue, $name?")
      answer <- getStrLn
      result <- answer.toLowerCase() match {
        case "y" => IO.pure(true)
        case _ => IO.pure(false)
      }
    } yield result

  def gameLoop(name: String): IO[Unit] =
    for {
      num   <- nextInt(5)
      _     <- putStrLn(s"$name, guess a number from 1 to 5:")
      input <- getStrLn
      _     <- parseInt(input).fold(
                 putStrLn("You guessed wrong!"))(
                 n => if (n == num) {
                   putStrLn(s"You guessed right, $name!")
                 } else {
                   putStrLn(s"You guessed wrong, $name! The number was $num.")
                 })
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else IO.pure(())
    } yield ()

  def main(): IO[Unit] = {

    for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Hello, $name, welcome to the game!")
      _    <- gameLoop(name)
    } yield ()
  }

}
