package org.greenrd.fixsymlinks

import java.nio.file.{Files, Path}

import scalaz.zio.{App, IO, Queue, Task, ZIO, ZQueue}
import scalaz.zio.console._

import collection.immutable.Set

object FixSymlinks extends App {

  def recreateSymbolicLink(source: Path, newDestination: Path): Task[Path] =
    (IO.effect(Files.deleteIfExists(source)) *> IO.effect(Files.createSymbolicLink(source, newDestination)))

  def askUserLoop(queue: Queue[(Path, Set[Path])]): ZIO[Console, Nothing, Unit] = {
    for {
      next <- queue.take
      _ <- next match {
        case (path, possibleDestinations) =>
          if(possibleDestinations.isEmpty) {
            putStrLn(s"ERROR: No target found for broken symlink $path")
          } else {
            IO.effect(assert(possibleDestinations.size > 1)).orDie *>
              putStrLn(s"${possibleDestinations.size} destinations found for $path:") *>
              (for {
                newDestination <- ConsoleUtils.choice(possibleDestinations)
                _ <- recreateSymbolicLink(path, newDestination).orDie
              } yield ())
          }
      }
    } yield ()
  }

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    // Fiber 1: finds any broken symlinks, tries to fix them automatically, returns any choices for user
    // Current fiber: Asks user to make symlink fixing choices and displays errors
    for {
      queue <- ZQueue.bounded[(Path, Set[Path])](1024)
      finderFiber <- (IO.succeed(???) *> queue.shutdown).fork
      _ <- askUserLoop(queue)
    } yield 0
  }
}
