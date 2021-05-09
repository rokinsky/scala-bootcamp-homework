package com.evolutiongaming.bootcamp.effects.minhash.error

import scala.util.control.NoStackTrace

sealed trait ValidationError extends Throwable with NoStackTrace
case object InvalidSeed extends ValidationError
case object InvalidFilepath extends ValidationError
