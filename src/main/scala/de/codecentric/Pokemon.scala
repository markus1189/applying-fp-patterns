package de.codecentric

import cats.data.{Xor, XorT}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DriverException extends Throwable // from external library

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
  case class FindOwnerFailed(owner: PokemonOwner.Id) extends TxError
  case class FindPokemonFailed(pokemon: Pokemon.Id) extends TxError
  case class DeletionFailed(id: Pokemon.Id, owner: PokemonOwner, e: Throwable) extends TxError

  case class SavingFailed(id: Pokemon, owner: PokemonOwner, e: Throwable) extends TxError
  case class SavingTimedOut(id: Pokemon.Id, owner: PokemonOwner, e: Throwable) extends TxError

  val repo: PokemonRepository = null
  val log = new { def info(s: String) = println(s) }

  trait PokemonRepository {
    def find(id: PokemonOwner.Id): Future[Xor[DriverException,PokemonOwner]]
    def find(id: Pokemon.Id): Future[Xor[DriverException,Pokemon]]
    def delete(id: Pokemon.Id, owner: PokemonOwner.Id): Future[Xor[DriverException,Unit]]
    def save(p: Pokemon, owner: PokemonOwner.Id): Future[Xor[DriverException,Unit]]
  }

  def transfer(
    from: PokemonOwner.Id,
    to: PokemonOwner.Id)(
    id: Pokemon.Id): Future[Xor[TxError,Unit]] = (for {
    fromOwner <- XorT(repo.find(from)).leftMap[TxError](_ => FindOwnerFailed(from))
    toOwner <- XorT(repo.find(to)).leftMap[TxError](_ => FindOwnerFailed(to))
    pokemon <- XorT(repo.find(id)).leftMap[TxError](_ => FindPokemonFailed(id))
    _ <- XorT(repo.delete(id, from)).leftMap[TxError](DeletionFailed(id, fromOwner, _))
    _ <- XorT(repo.save(pokemon, to)).leftMap[TxError](SavingFailed(pokemon, toOwner, _))
  } yield {
    log.info(
      s"Transferred pokemon ${pokemon.name}: ${fromOwner.name} -> ${toOwner.name}")
  }).value
}
