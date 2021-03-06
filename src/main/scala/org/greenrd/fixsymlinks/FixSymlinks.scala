package org.greenrd.fixsymlinks

import java.io.IOException
import java.nio.file.{LinkOption, Path, Paths}

import scalaz.deriving
import scalaz.Monoid
import scalaz.Scalaz._
import scalaz.zio.{App, IO, Queue, Task, UIO, ZIO, ZQueue}
import scalaz.zio.console._

import collection.immutable.Set

import nequi.zio.logger
import nequi.zio.logger.Slf4jLogger

import org.greenrd.fixsymlinks.modules.Files

@deriving(Monoid)
case class ScanResults(symlinks: List[Path], directories: List[Path])

case class Stats(nSymLinks: Int) extends AnyVal {
  override def toString = s"$nSymLinks symbolic links"
}

object FixSymlinks extends App with Slf4jLogger {

  override val clazz = sourcecode.File()

  /** Left indicates no more symlinks found. */
  type SymlinkData = Either[Stats, (Path, Set[Path])]

  def recreateSymbolicLink(source: Path, newDestination: Path): ZIO[Files, Nothing, Path] = (for {
    files <- ZIO.access[Files](_.files)
    _ <- files.deleteIfExists(source)
    p <- files.createSymbolicLink(source, newDestination)
  } yield p).orDie

  /** Asks user to make symlink fixing choices and displays errors.
    * 
    * @param The queue to read choices and failures from
    */
  def askUserLoop(queue: Queue[SymlinkData]): ZIO[Console with Files, Nothing, Unit] = {
    for {
      next <- queue.take
      _ <- next match {
        case Right((path, possibleDestinations)) =>
          (possibleDestinations.headOption match {
            case Some(only) if possibleDestinations.size == 1 =>
              logger.info(s"Repointed broken symlink $path to $only")
            case None =>
              logger.error(s"No target found for broken symlink $path")
            case _ =>
              logger.info(s"${possibleDestinations.size} destinations found for $path:") *>
              (for {
                newDestination <- ConsoleUtils.choice(possibleDestinations)
                _ <- recreateSymbolicLink(path, newDestination).orDie
              } yield ())
          }) *> askUserLoop(queue)
        case Left(stats) =>
          logger.info(s"Done - scanned $stats")
      }
    } yield ()
  }

  /** Searches until it finds an ancestor path that exists.
    * 
    * @param path A non-existing path
    */
  def findAncestorThatExists(path: Path): ZIO[Files, IOException, Path] = {
    for {
      parent <- Option(path.getParent).map(IO.succeed).getOrElse(IO.fail(new IOException(s"Root $path does not exist")))
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
  def scanAndTryToFix(paths: List[Path], queue: Queue[SymlinkData]): ZIO[Files, Nothing, Stats] = {

    def tryToFix(symlink: Path, oldDest: Path): ZIO[Files, Nothing, Unit] = {
      val name = oldDest.getFileName
      for {
        ancestorThatExists <- findAncestorThatExists(oldDest).orDie
        possibilities <- findFilesNamed(name)(ancestorThatExists).orDie
        _ <- possibilities.headOption match {
          case Some(only) if possibilities.size == 1 =>
            recreateSymbolicLink(symlink, only)
          case _ =>
            IO.succeed(())
        }
        _ <- queue.offer(Right(symlink -> possibilities))
      } yield ()
    }

    def process(path: Path): ZIO[Files, Nothing, Stats] = {
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
            absDest = symlink.resolveSibling(dest)
            _ <- tryToFix(symlink, absDest).whenM(files.exists(absDest).orDie.map(!_))
          } yield ()
        }
        subdirs <- scanAndTryToFix(scanResults.directories, queue)
      } yield Stats(scanResults.symlinks.size + subdirs.nSymLinks)
    }

    ZIO.foldLeft(paths)(Stats(0)) { (count, path) => process(path).map(s => Stats(count.nSymLinks + s.nSymLinks)) }
  }

  def runImpl(args: List[String]): ZIO[Console with Files, Nothing, Int] =
    for {
      queue <- ZQueue.bounded[SymlinkData](1024)
      files <- ZIO.access[Files](_.files)
      dirs <- ZIO.foreach(args)(arg => files.parsePath(arg).orDie)
      scanFiber <- (for {
        nSymLinks <- scanAndTryToFix(dirs, queue)
        _ <- queue.offer(Left(nSymLinks))
      } yield ()).fork
      _ <- askUserLoop(queue)
    } yield 0

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    val pathArgs = args.filterNot(_.startsWith("-"))
    if(args.contains("--help") || args.contains("-h")) {
      putStrLn("-h\tDisplay this help") *>
      putStrLn("-n\tDry run - do not touch the filesystem, just print out what would be done") *> IO.succeed(0)
    } else if(args.contains("-n")) {
      logger.info("Dry run mode! Any changes shown below have not actually been performed!") *>
      runImpl(pathArgs).provideSome(c => new Console with Files.DryRun {
        override val console = c.console
      })
    } else {
      runImpl(pathArgs).provideSome(c => new Console with Files.Live {
        override val console = c.console
      })
    }
  }
}
