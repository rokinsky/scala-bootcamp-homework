package com.evolutiongaming.bootcamp.effects.minhash.domain

import com.evolutiongaming.bootcamp.effects.minhash.error.{InvalidFilepath, ValidationError}

import java.io.{File => JFile}

sealed abstract case class File private (value: JFile)
object File {
  def from(value: String): Either[ValidationError, File] = {
    val jFile = new JFile(value)
    Either.cond(jFile.isFile && jFile.length() > 0, new File(jFile) {}, InvalidFilepath)
  }

  implicit val parseFile: Parse[File] = from
}
