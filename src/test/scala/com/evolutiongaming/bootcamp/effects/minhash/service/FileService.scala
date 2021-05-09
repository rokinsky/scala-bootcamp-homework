package com.evolutiongaming.bootcamp.effects.minhash.service

import cats.effect.{Blocker, ContextShift, Resource, Sync}
import com.evolutiongaming.bootcamp.effects.minhash.domain.File

import scala.io.Source

class FileService[F[_]: Sync: ContextShift](source: Source, blocker: Blocker) {
  def words: F[List[String]] = blocker.delay {
    source
      .getLines()
      .flatMap(_.split("\\W+"))
      .toList
  }
}

object FileService {
  def of[F[_]: Sync: ContextShift](file: File, blocker: Blocker): Resource[F, FileService[F]] =
    Resource(blocker.delay {
      val source = Source.fromFile(file.value)
      (new FileService[F](source, blocker), blocker.delay(source.close()))
    })
}
