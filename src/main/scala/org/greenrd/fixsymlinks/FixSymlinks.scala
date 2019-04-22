package org.greenrd.fixsymlinks

import java.io.IOException
import java.nio.file.{LinkOption, Path, Paths}

import scalaz.deriving
import scalaz.Monoid
import scalaz.Scalaz._
import scalaz.zio.{App, IO, Queue, Task, UIO, ZIO, ZQueue}
import scalaz.zio.console._

import collection.immutable.Set

import org.greenrd.fixsymlinks.modules.Files

@deriving(Monoid)
case class ScanResults(symlinks: List[Path], directories: List[Path])

object FixSymlinks extends App {

  def recreateSymbolicLink(source: Path, newDestination: Path): ZIO[Files, Nothing, Path] = (for {
    files <- ZIO.access[Files](_.files)
    _ <- files.deleteIfExists(source)
    p <- files.createSymbolicLink(source, newDestination)
  } yield p).orDie

  /** Asks user to make symlink fixing choices and displays errors.
    * 
    * @param The queue to read choices and failures from
    */
  def askUserLoop(queue: Queue[(Path, Set[Path])]): ZIO[Console with Files, Nothing, Unit] = {
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

  /** Searches until it finds an ancestor path that exists.
    * 
    * @param path A non-existing path
    */
  def findAncestorThatExists(path: Path): ZIO[Files, IOException, Path] = {
    val parent = path.getParent
    for {
      files <- ZIO.access[Files](_.files)
      parentExists <- files.exists(parent)
      result <- if(parentExists) IO.succeed(parent) else findAncestorThatExists(parent)
    } yield result
  }

  def findFilesNamed(name: Path): Path => ZIO[Files, IOException, Set[Path]] = { p =>
    ZIO.access[Files](_.files).flatMap { files =>
      def me(under: Path): ZIO[Any, IOException, Set[Path]] = {
        files.fold(under) { path =>
          for {
            attrs <- files.readBasicAttributes(path, Set(LinkOption.NOFOLLOW_LINKS))
            r <- if(attrs.isDirectory) {
              me(path)
            } else IO.succeed(if(path.getFileName == name) Set(path) else Set[Path]())
          } yield r
        }
      }
      me(p)
    }
  }

  /** Finds any broken symlinks, tries to fix them automatically, enqueues any choices and failures for user.
    * 
    * @param paths The directories to scan recursively for broken symlinks
    * @param queue The queue to enqueue choices and failures into
    */
  def scanAndTryToFix(paths: List[Path], queue: Queue[(Path, Set[Path])]): ZIO[Files, Nothing, Int] = {

    def tryToFix(symlink: Path, oldDest: Path): ZIO[Files, Nothing, Unit] = {
      val name = oldDest.getFileName
      for {
        ancestorThatExists <- findAncestorThatExists(oldDest).orDie
        possibilities <- findFilesNamed(name)(ancestorThatExists).orDie
        _ <- possibilities.headOption match {
          case Some(only) if possibilities.size == 1 =>
            recreateSymbolicLink(symlink, only)
          case _ =>
            queue.offer(symlink -> possibilities)
        }
      } yield ()
    }

    def process(path: Path): ZIO[Files, Nothing, Int] = {
      val z = Monoid[ScanResults].zero
      for {
        files <- ZIO.access[Files](_.files)
        scanResults <- files.fold(path) { path =>
          val name = path.getFileName
          if(name == "." || name == "..") IO.succeed(z)
          else
          for {
            attrs <- files.readBasicAttributes(path, Set(LinkOption.NOFOLLOW_LINKS))
          } yield {
            if(attrs.isSymbolicLink) ScanResults(List(path), List())
            else if(attrs.isDirectory) ScanResults(List(), List(path))
            else z
          }
        }.orDie
        _ <- ZIO.foreach(scanResults.symlinks) { symlink =>
          for {
            dest <- files.readSymbolicLink(symlink).orDie
            _ <- tryToFix(symlink, dest).whenM(files.exists(dest).orDie.map(!_))
          } yield ()
        }
        subdirCount <- scanAndTryToFix(scanResults.directories, queue)
      } yield scanResults.symlinks.size + subdirCount
    }

    ZIO.foldLeft(paths)(0) { (count, path) => process(path).map(count + _) }
  }

  def runImpl(args: List[String]): ZIO[Console with Files, Nothing, Int] =
    for {
      queue <- ZQueue.bounded[(Path, Set[Path])](1024)
      files <- ZIO.access[Files](_.files)
      dirs <- ZIO.foreach(args)(arg => files.parsePath(arg).orDie)
      nSymLinks <- (for {
        interactiveFiber <- askUserLoop(queue).fork
        count <- scanAndTryToFix(dirs, queue)
      } yield count).supervise
      _ <- putStrLn(s"Done - scanned $nSymLinks symbolic links")
    } yield 0

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    runImpl(args).provideSome[Console](c => new Console with Files.Live {
        override val console = c.console
      })
  }
}
