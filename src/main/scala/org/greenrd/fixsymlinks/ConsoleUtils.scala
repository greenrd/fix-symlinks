package org.greenrd.fixsymlinks

import scalaz.zio.{IO, TaskR, ZIO}
import scalaz.zio.console._

object ConsoleUtils {

  def choice[T](choices: Iterable[T]): ZIO[Console, Nothing, T] = {
    def retry(error: String): ZIO[Console, Nothing, T] = putStrLn(s"ERROR: $error") *> choice(choices)
    def handler(error: ChoiceError) = retry(error match {
      case NotInt => "Not an Int"
      case OutOfRange(selected) => s"The number you entered, $selected, was not in the list"
    })
    val numberedChoices = choices.zipWithIndex
    val result: ZIO[Console, ChoiceError, T] = ZIO.foreach(numberedChoices)(nc => putStrLn(nc.toString)) *>
      putStrLn("Please make your choice by entering the number next to your selection:") *>
      (for {
        selected <- getInt
        selectedValue <- numberedChoices.find(_._2 == selected).fold[ZIO[Console, ChoiceError, T]](IO.fail(OutOfRange(selected)))(sv => IO.succeed(sv._1))
      } yield selectedValue)
    result.either.flatMap(_.fold(handler, ZIO.succeed))
  }

  def getInt: ZIO[Console, NotInt.type, Int] = {
    for {
      line <- getStrLn.orDie
      selected <- IO.effect(line.toInt).catchAll[Console, NotInt.type, Int](_ => ZIO.fail(NotInt))
    } yield selected
  }

}

sealed trait ChoiceError extends Throwable
case object NotInt extends ChoiceError
case class OutOfRange(selected: Int) extends ChoiceError