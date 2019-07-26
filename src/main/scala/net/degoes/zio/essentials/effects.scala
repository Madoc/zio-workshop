// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials

import java.util.Scanner

object effects {

  /**
   * `Console` is an immutable data structure that describes a console program
   * that may involve reading from the console, writing to the console, or
   * returning a value.
   */
  sealed trait Console[A] { self =>
    import Console._

    /**
     * Implement `flatMap` for every type of `Console[A]` to turn it into a
     * `Console[B]` using the function `f`.
     */
    final def flatMap[B](f: A => Console[B]): Console[B] = this match {
      case ReadLine(next)        => ReadLine(next.andThen(_.flatMap(f)))
      case WriteLine(line, next) => WriteLine(line, next.flatMap(f))
      case Return(value)         => f(value())
    }

    final def map[B](f: A => B): Console[B] = flatMap(f andThen (Console.succeed(_)))

    final def *>[B](that: Console[B]): Console[B] = (self zip that).map(_._2)

    final def <*[B](that: Console[B]): Console[A] = (self zip that).map(_._1)

    /**
     * Implement the `zip` function using `flatMap` and `map`.
     */
    final def zip[B](that: Console[B]): Console[(A, B)] = flatMap(a => that.map(b => (a, b)))
  }
  object Console {
    final case class ReadLine[A](next: String => Console[A])      extends Console[A]
    final case class WriteLine[A](line: String, next: Console[A]) extends Console[A]
    final case class Return[A](value: () => A)                    extends Console[A]

    /**
     * Implement the following helper functions:
     */
    final val readLine: Console[String]              = ReadLine(string => succeed(string))
    final def writeLine(line: String): Console[Unit] = WriteLine(line, succeed(()))
    final def succeed[A](a: => A): Console[A]        = Return(() => a)
  }

  /**
   * Using the helper functions, write a program that just returns a unit value.
   */
  val unit: Console[Unit] = Console.succeed(())

  /**
   * Using the helper functions, write a program that just returns the value 42.
   */
  val fortyTwo: Console[Int] = Console.succeed(42)

  /**
   * Using the helper functions, write a program that asks the user for their name.
   */
  val askName: Console[Unit] = Console.writeLine("What is your name?")

  /**
   * Using the helper functions, write a program that read the name of the user.
   */
  val readName: Console[String] = Console.readLine

  /**
   * Using the helper functions, write a program that greets the user by their name.
   */
  def greetUser(name: String): Console[Unit] = Console.writeLine(s"Hello, $name!")

  /***
   * Using `flatMap` and the preceding three functions, write a program that
   * asks the user for their name, reads their name, and greets them.
   */
  val sayHello: Console[Unit] = askName.flatMap(_ => readName.flatMap(name => greetUser(name)))

  /**
   * Write a program that reads from the console then parse the given input into int if it possible
   * otherwise it returns None
   */
  val readInt: Console[Option[Int]] =
    Console.readLine.map(line => try { Some(line.toInt) } catch { case _: NumberFormatException => None })

  /**
   * implement the following effectful procedure, which interprets
   * the description of a given `Console[A]` into A and run it.
   */
  def unsafeRun[A](program: Console[A]): A = program match {
    case Console.ReadLine(next)        => unsafeRun(next(new Scanner(System.in).nextLine()))
    case Console.WriteLine(line, next) => println(line); unsafeRun(next)
    case Console.Return(value)         => value()
  }

  /**
   * implement the following combinator `collectAll` that operates on programs
   */
  def collectAll[A](programs: List[Console[A]]): Console[List[A]] = programs match {
    case Nil          => Console.succeed(Nil)
    case head :: tail => head.flatMap(headA => collectAll(tail).map(tailAs => headA :: tailAs))
  }

  /**
   * implement the `foreach` function that compute a result for each iteration
   */
  def foreach[A, B](values: List[A])(body: A => Console[B]): Console[List[B]] = collectAll(values.map(body))

  /**
   * Using `Console.writeLine` and `Console.readLine`, map the following
   * list of strings into a list of programs, each of which writes a
   * question and reads an answer.
   */
  val questions =
    List(
      "What is your name?",
      "Where where you born?",
      "Where do you live?",
      "What is your age?",
      "What is your favorite programming language?"
    )
  val answers: List[Console[String]] =
    questions.map(question => Console.writeLine(question).flatMap(_ => Console.readLine))

  /**
   * Using `collectAll`, transform `answers` into a program that returns
   * a list of strings.
   */
  val answers2: Console[List[String]] = collectAll(answers)

  /**
   * Now using only `questions` and `foreach`, write a program that is
   * equivalent to `answers2`.
   */
  val answers3: Console[List[String]] = foreach(questions) { question =>
    Console.writeLine(question).flatMap(_ => Console.readLine)
  }

  /**
   * Implement the methods of Thunk
   */
  class Thunk[A](val unsafeRun: () => A) {
    def map[B](ab: A => B): Thunk[B]             = new Thunk(() => ab(unsafeRun()))
    def flatMap[B](afb: A => Thunk[B]): Thunk[B] = new Thunk(() => afb(unsafeRun()).unsafeRun())
    def attempt: Thunk[Either[Throwable, A]] =
      new Thunk(() => try { Right(unsafeRun()) } catch { case t: Throwable => Left(t) })
  }
  object Thunk {
    def succeed[A](a: => A): Thunk[A]   = new Thunk(() => a)
    def fail[A](t: Throwable): Thunk[A] = new Thunk(() => throw t)
  }

  /**
   * Build the version of printLn and readLn
   * then make a simple program base on that.
   */
  def printLn(line: String): Thunk[Unit] = new Thunk(() => println(line))
  def readLn: Thunk[String]              = new Thunk(() => new Scanner(System.in).nextLine())

  val thunkProgram: Thunk[Unit] =
    printLn("What is your name?").flatMap(_ => readLn).flatMap(name => printLn(s"Hello, $name!"))
}
