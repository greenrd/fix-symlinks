package org.greenrd.fixsymlinks.modules

import java.io.{FileNotFoundException, IOException, UncheckedIOException}
import java.nio.file.{Files => JFiles, InvalidPathException, LinkOption, Path, Paths}
import java.nio.file.attribute.BasicFileAttributes
import java.util.stream.Stream

import scalaz.Monoid
import scalaz.Scalaz._
import scalaz.zio.{DefaultRuntime, IO, ZIO}

import collection.immutable.{Seq, Set}
import scala.collection.JavaConverters._

trait Files {
  val files: Files.Service[Any]
}

object Files {
  trait Service[R] {
    def parsePath(s: String): ZIO[R, InvalidPathException, Path]

    def deleteIfExists(p: Path): ZIO[R, IOException, Boolean]

    def createSymbolicLink(src: Path, dest: Path): ZIO[R, IOException, Path]

    def readBasicAttributes(p: Path, options: Set[LinkOption] = Set()): ZIO[R, IOException, BasicFileAttributes]

    def exists(p: Path): ZIO[R, IOException, Boolean]

    def readSymbolicLink(p: Path): ZIO[R, IOException, Path]

    def fold[T: Monoid](p: Path)(f: Path => ZIO[R, IOException, T]): ZIO[R, IOException, T]
  }

  trait Live extends Files {
    val files: Service[Any] = new Service[Any] {

      val runtime = new DefaultRuntime {}

      override def parsePath(s: String) = IO.effect(Paths.get(s)).refineOrDie {
        case e: InvalidPathException => e
      }

      override def deleteIfExists(p: Path) = IO.effect(JFiles.deleteIfExists(p)).refineOrDie {
        case e: IOException => e
      }

      override def createSymbolicLink(src: Path, dest: Path) = IO.effect(JFiles.createSymbolicLink(src, dest)).refineOrDie {
        case e: IOException => e
      }

      override def readBasicAttributes(p: Path, options: Set[LinkOption] = Set()) =
        IO.effect(JFiles.readAttributes(p, classOf[BasicFileAttributes], options.toArray: _*)).refineOrDie {
          case e: IOException => e
        }

      override def exists(p: Path) = readBasicAttributes(p).foldM({
          case fnf: FileNotFoundException => IO.succeed(false)
          case ex => IO.fail(ex)
        }, _ => IO.succeed(true))

      override def readSymbolicLink(p: Path) = IO.effect(JFiles.readSymbolicLink(p)).refineOrDie {
        case e: IOException => e
      }

      override def fold[T: Monoid](p: Path)(f: Path => ZIO[Any, IOException, T]) = {
        val dirStreamIO: ZIO[Any, IOException, Stream[Path]] = IO.effect(JFiles.list(p)).refineOrDie {
          case e: IOException => e
        }
        dirStreamIO.bracket(stream => IO.effect(stream.close).orDie) { dirStream =>
          IO.effect {
            dirStream.map[T](path => runtime.unsafeRun(f(path))).reduce(Monoid[T].zero, _ ⊹ _)
          }.refineOrDie {
            case e: IOException => e
            case u: UncheckedIOException => u.getCause
          }
        }
      }
    }
  }
  object Live extends Live
}
