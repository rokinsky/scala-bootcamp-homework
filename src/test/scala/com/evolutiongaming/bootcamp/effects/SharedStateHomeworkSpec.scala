package com.evolutiongaming.bootcamp.effects

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{ExitCode, IO}
import com.evolutiongaming.bootcamp.effects.SharedStateHomework.Cache
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class SharedStateHomeworkSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "Cache" should "correctly leave two caches" in {
    val io = Cache
      .of[IO, Int, String](400.millis, 300.millis)
      .use(cache =>
        for {
          _  <- cache.put(1, "a")
          _  <- cache.put(2, "b")
          _  <- IO.sleep(300.millis)
          v1 <- cache.get(1)
          _  <- IO(assert(v1.isDefined))
          v2 <- cache.get(2)
          _  <- IO(assert(v2.isDefined))
        } yield ExitCode.Success
      )
    assert(io.unsafeRunSync() == ExitCode.Success)
  }

  it should "correctly evict two caches" in {
    val io = Cache
      .of[IO, Int, String](400.millis, 300.millis)
      .use(cache =>
        for {
          _  <- cache.put(1, "a")
          _  <- cache.put(2, "b")
          _  <- IO.sleep(800.millis)
          v1 <- cache.get(1)
          _  <- IO(assert(v1.isEmpty))
          v2 <- cache.get(2)
          -  <- IO(assert(v2.isEmpty))
        } yield ExitCode.Success
      )
    assert(io.unsafeRunSync() == ExitCode.Success)
  }

  it should "correctly renew key expiration" in {
    val io = Cache
      .of[IO, Int, String](400.millis, 300.millis)
      .use(cache =>
        for {
          _  <- cache.put(1, "a")
          _  <- cache.put(2, "b")
          _  <- IO.sleep(300.millis)
          _  <- cache.put(2, "b")
          _  <- IO.sleep(300.millis)
          v1 <- cache.get(1)
          _  <- IO(assert(v1.isEmpty))
          v2 <- cache.get(2)
          _  <- IO(assert(v2.isDefined))
        } yield ExitCode.Success
      )
    assert(io.unsafeRunSync() == ExitCode.Success)
  }
}
