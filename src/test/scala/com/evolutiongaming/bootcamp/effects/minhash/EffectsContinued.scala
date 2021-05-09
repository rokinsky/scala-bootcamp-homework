package com.evolutiongaming.bootcamp.effects.minhash

import cats.Parallel
import cats.effect.{Blocker, Concurrent, ContextShift, ExitCode, IO, IOApp, Resource, Sync}
import com.evolutiongaming.bootcamp.effects.minhash.domain.{File, Seed}
import com.evolutiongaming.bootcamp.effects.minhash.repository.SignatureFakeRepository
import com.evolutiongaming.bootcamp.effects.minhash.service.{FileService, HashService, ParseService, SignatureService}

/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.

  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0

    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
 */
object EffectsContinued extends IOApp {
  def app[F[_]: Sync: ContextShift: Concurrent: Parallel]: Resource[F, Unit] = {
    for {
      blocker          <- Blocker.apply[F]
      parseService     <- ParseService.of[F](blocker)
      file             <- Resource.liftF(parseService.parse[File]("Enter filepath: "))
      seed             <- Resource.liftF(parseService.parse[Seed]("Enter seed: "))
      fileService      <- FileService.of[F](file, blocker)
      signatureRepo    <- SignatureFakeRepository.of[F]
      signatureService <- SignatureService.of[F](signatureRepo)
      words            <- Resource.liftF(fileService.words)
      hashes           <- Resource.liftF(HashService.hashes[F](seed))
      _                <- Resource.liftF(signatureService.sign(file, words, hashes))
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    app[IO].use(IO.pure).as(ExitCode.Success)
  }
}
