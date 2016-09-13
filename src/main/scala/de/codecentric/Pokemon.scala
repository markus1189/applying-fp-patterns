package de.codecentric

import cats.data.{Xor, XorT}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DriverException extends Throwable // from external library
class QueryTimedOutException extends DriverException

case class Pokemon(id: Int, typ: Type, name: String)
object Pokemon {
  case class Id(value: Int)
}
sealed trait Type extends Product with Serializable
case object Fire extends Type
case object Water extends Type
// ...

case class PokemonOwner(name: String)
object PokemonOwner {
  case class Id(value: Int)
}

object NotFunctional {
  trait PokemonRepository {
    def find(id: PokemonOwner.Id): Future[PokemonOwner]
    def find(id: Pokemon.Id): Future[Pokemon]
    def delete(id: Pokemon.Id, owner: PokemonOwner.Id): Future[Unit]
    def save(p: Pokemon, owner: PokemonOwner.Id): Future[Unit]
  }

  val repo: PokemonRepository = null
  val log = new { def info(s: String) = println(s) }

  def transfer(from: PokemonOwner.Id,
    to: PokemonOwner.Id)(
    id: Pokemon.Id): Future[Unit] = for {
      fromOwner <- repo.find(from)
      toOwner <- repo.find(to)
      pokemon <- repo.find(id)
      _ <- repo.delete(id, from)
      _ <- repo.save(pokemon, to)
  } yield {
    log.info(
      s"Transferred pokemon ${pokemon.name}: ${fromOwner.name} -> ${toOwner.name}")
  }
}

object Functional {
  sealed trait TxError extends Product with Serializable
  case class Domain(e: DomainError) extends TxError
  case class External(e: ExternalError) extends TxError

  sealed trait DomainError
  case class OwnerNotFound(id: PokemonOwner.Id) extends DomainError
  case class PokemonNotFound(id: Pokemon.Id) extends DomainError

  sealed trait ExternalError
  case class FindOwnerFailed(id: PokemonOwner.Id, e: Throwable) extends ExternalError
  case class FindPokemonFailed(id: Pokemon.Id, e: Throwable) extends ExternalError
  case class DeletionFailed(id: Pokemon.Id, owner: PokemonOwner.Id, e: Throwable) extends ExternalError
  case class SavingFailed(id: Pokemon, owner: PokemonOwner.Id, e: Throwable) extends ExternalError

  val repo: PokemonRepository = null
  val log = new { def info(s: String) = println(s) }

  trait PokemonRepository {
    def find(id: PokemonOwner.Id): Future[Xor[DriverException,Option[PokemonOwner]]]
    def find(id: Pokemon.Id): Future[Xor[DriverException,Option[Pokemon]]]
    def delete(id: Pokemon.Id, owner: PokemonOwner.Id): Future[Xor[DriverException,Unit]]
    def save(p: Pokemon, owner: PokemonOwner.Id): Future[Xor[DriverException,Unit]]
  }

  def safeFind(id: PokemonOwner.Id): XorT[Future, TxError, PokemonOwner] = {
    XorT(repo.find(id)).
      leftMap[TxError](e => External(FindOwnerFailed(id,e))).
      subflatMap(opt => Xor.fromOption(opt, Domain(OwnerNotFound(id))))
  }

  def safeFind(id: Pokemon.Id): XorT[Future, TxError, Pokemon] = {
    XorT(repo.find(id)).
      leftMap[TxError](e => External(FindPokemonFailed(id,e))).
      subflatMap(opt => Xor.fromOption(opt, Domain(PokemonNotFound(id))))
  }

  def safeDelete(id: Pokemon.Id, owner: PokemonOwner.Id): XorT[Future, TxError, Unit] = {
    XorT(repo.delete(id, owner)).
      leftMap[TxError](e => External(DeletionFailed(id, owner, e)))
  }

  def safeSave(p: Pokemon, owner: PokemonOwner.Id) = {
    XorT(repo.save(p, owner)).
      leftMap[TxError](e => External(SavingFailed(p, owner, e)))
  }

  def transfer(
    from: PokemonOwner.Id,
    to: PokemonOwner.Id)(
    id: Pokemon.Id): Future[Xor[TxError,Unit]] = (for {
    fromOwner <- safeFind(from)
    toOwner <- safeFind(to)
    pokemon <- safeFind(id)
    _ <- safeDelete(id,from)
    _ <- safeSave(pokemon, to)
  } yield {
    log.info(
      s"Transferred pokemon ${pokemon.name}: ${fromOwner.name} -> ${toOwner.name}")
  }).value

  val o1 = PokemonOwner.Id(1)
  val o2 = PokemonOwner.Id(2)
  val p = Pokemon.Id(3)

  transfer(o1,o2)(p) map {
    case Xor.Right(()) => """\o/"""
    case Xor.Left(Domain(domain)) => domain match {
      case OwnerNotFound(id) =>   // display error
      case PokemonNotFound(id) => // display error
    }
    case Xor.Left(External(external)) => external match {
      case SavingFailed(id, owner, e: QueryTimedOutException) => // UH OH! Retry?
      case SavingFailed(_,_,e) => // log error and display generic error
      case _ => // some generic handling
    }
  }
}
