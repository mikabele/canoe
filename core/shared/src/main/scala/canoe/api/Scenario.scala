package canoe.api

import canoe.api.matching.Episode
import canoe.models.Messageable
import cats.{MonadThrow, ~>}
import fs2.Pipe

import scala.concurrent.duration.FiniteDuration

/** Description of an interaction between two parties,
  * where generally one is the application (bot) and the other is Telegram user.
  *
  * Particular interaction is coherent with some scenario as long as it matches the description
  * (i.e. every step of the interaction matches the step described in scenario).
  *
  * `Scenario` forms a monad in `A` with `pure` and `flatMap`.
  */
final class Scenario[F[_], +A] private (private val ep: Episode[F, Messageable, A]) extends AnyVal {
  def pipe(implicit F: MonadThrow[F]): Pipe[F, Messageable, A] =
    ep.matching

  /** Chains this scenario with the one produced by applying `fn` to the result of this scenario.
    */
  def flatMap[B](fn: A => Scenario[F, B]): Scenario[F, B] =
    new Scenario[F, B](ep.flatMap(fn(_).ep))

  /** Lazy `flatMap` which ignores the result of the first scenario.
    */
  def >>[B](s2: => Scenario[F, B]): Scenario[F, B] = flatMap(_ => s2)

  /** Maps successful result values using provided function.
    */
  def map[B](fn: A => B): Scenario[F, B] = flatMap(fn.andThen(Scenario.pure(_)))

  /** @return `this` or scenario which is result of provided function if error occurs.
    */
  def handleErrorWith[A2 >: A](fn: Throwable => Scenario[F, A2]): Scenario[F, A2] =
    new Scenario[F, A2](Episode.Protected(ep, fn(_).ep))

  /** @return models.MyScenario which wraps successful result values in `Right` and raised errors in `Left`.
    */
  def attempt: Scenario[F, Either[Throwable, A]] =
    map(Right(_): Either[Throwable, A]).handleErrorWith(e => Scenario.pure(Left(e)))

  /** Restarts this scenario if it was mismatched, up to `n` times,
    * evaluating `fn` applied to the input element that was mismatched.
    *
    * This can be useful, when you expect some input from the user (e.g. data in specific format)
    * and want to retry if the input was not correct.
    */
  def tolerateN(n: Int)(fn: Messageable => F[Unit]): Scenario[F, A] =
    new Scenario[F, A](Episode.Tolerate(ep, Some(n), fn))

  /** Alias for tolerateN(1) */
  def tolerate(fn: Messageable => F[Unit]): Scenario[F, A] = tolerateN(1)(fn)

  /** Same as tolerateN, but retries until the scenario is fully matched.
    *
    * Often used in combination with stopOn/stopWith to give users a way to 'escape' the scenario.
    */
  def tolerateAll(fn: Messageable => F[Unit]): Scenario[F, A] =
    new Scenario[F, A](Episode.Tolerate(ep, None, fn))

  /** Stops this scenario on first input message matching the predicate.
    */
  def stopOn(p: Messageable => Boolean): Scenario[F, A] =
    new Scenario[F, A](Episode.Cancellable(ep, p, None))

  /** Stops this scenario on first input message matching the predicate
    * and evaluates cancellation function with this message.
    */
  def stopWith(p: Messageable => Boolean)(cancellation: Messageable => F[Unit]): Scenario[F, A] =
    new Scenario[F, A](Episode.Cancellable(ep, p, Some(cancellation)))

  /** Limit the amount of time that this scenario can run for.
    * Useful in case you want to no longer wait for user input after a certain period of time
    * and prefer to just drop the current interaction.
    *
    * models.MyScenario that has exceeded specified duration is going to be interrupted and finish with ExitCase.Canceled.
    */
  def within(duration: FiniteDuration): Scenario[F, A] =
    new Scenario[F, A](Episode.TimeLimited(ep, duration))

  /** Maps effect type from `F` to `G` using the supplied transformation.
    *
    * Warning: this operation can result into StackOverflowError
    * if `this` is nested with a lot of `flatMap` operations.
    */
  def mapK[G[_]](f: F ~> G): Scenario[G, A] =
    new Scenario[G, A](ep.mapK(f))}

object Scenario extends ScenarioInstances {

  /** Describes the next expected received message.
    *
    * Any input message from `pf` domain will be matched
    * and transformed into a value of type `A`.
    */
  def expect[F[_], A](pf: PartialFunction[Messageable, A]): Scenario[F, A] =
    new Scenario[F, A](Episode.Next(pf.isDefinedAt).map(pf))

  /** Suspends an effectful value of type `A` into models.MyScenario context.
    *
    * Generally used for describing action that should be executed by the bot
    * (e.g. sending messages, making calls to external APIs, etc.)
    */
  def eval[F[_], A](fa: F[A]): Scenario[F, A] =
    new Scenario[F, A](Episode.Eval(fa))

  /** Lifts pure value to models.MyScenario context.
    *
    * Uses partially applied type parameter technique.
    */
  def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]

  final class PurePartiallyApplied[F[_]](private val dummy: Boolean = false) extends AnyVal {

    /** Lifts pure value to Scenario context.
      */
    def apply[A](a: A): Scenario[F, A] = new Scenario[F, A](Episode.Pure(a))
  }

  /** Unit value lifted to models.MyScenario context with effect `F`.
    */
  def done[F[_]]: Scenario[F, Unit] = pure(())

  /** Lifts error value to the models.MyScenario context.
    *
    * Error can be safely brought back to the return value domain using `attempt` method.
    * It also can be handled using various methods from `MonadError`
    * such as `handleErrorWith`, `recover` etc.
    *
    * @return models.MyScenario which fails with `e`
    */
  def raiseError[F[_]](e: Throwable): Scenario[F, Nothing] =
    new Scenario[F, Nothing](Episode.RaiseError(e))

  def fromEitherF[F[_], A, B <: Throwable](f: F[Either[B, A]]): Scenario[F, A] = {
    for {
      temp <- Scenario.eval(f)
      res <- temp.fold(
        error => Scenario.raiseError(error): Scenario[F, A],
        value => Scenario.pure(value): Scenario[F, A]
      )
    } yield res
  }
}
