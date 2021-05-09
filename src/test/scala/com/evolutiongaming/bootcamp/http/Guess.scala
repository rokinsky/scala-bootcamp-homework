package com.evolutiongaming.bootcamp.http

import cats.effect.{ExitCode, IO, IOApp}
import com.evolutiongaming.bootcamp.http.client.{GuessHttpClient, GuessWsClient}
import com.evolutiongaming.bootcamp.http.server.GameServer

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.
object GuessServerApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    GameServer.resource[IO].use(_ => IO.never).as(ExitCode.Success)
}

object GuessHttpClientApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    GuessHttpClient.resource[IO].use(IO.pure).as(ExitCode.Success)
}

object GuessWsClientApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    GuessWsClient.resource[IO].use(IO.pure).as(ExitCode.Success)
}
