import cats.Traverse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import cats.implicits._

val either: Either[String,Future[Int]] = Right(Future(1))
//either.sequenceU

// kind-projector
Traverse[Lambda[A=>Either[String,A]]].sequence(either)
