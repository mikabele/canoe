package canoe.api.clients

import canoe.api.{FailedMethod, ResponseDecodingError, TelegramClient}
import canoe.methods.Method
import canoe.models.{InputFile, Response => TelegramResponse}
import cats.effect.Concurrent
import cats.syntax.all._
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.multipart.{Multipart, Part}

private[api] class Http4sTelegramClient[F[_]: Concurrent: Logger](token: String, client: Client[F])
    extends TelegramClient[F]
    with Http4sClientDsl[F] {

  private val botApiUri: Uri = Uri.unsafeFromString("https://api.telegram.org") / s"bot$token"

  def execute[Req, Res](request: Req)(implicit M: Method[Req, Res]): F[Res] = {

    val req = prepareRequest(botApiUri / M.name, M, request)

    implicit val decoder: EntityDecoder[F, TelegramResponse[Res]] =
      jsonOf(Concurrent[F], TelegramResponse.decoder(M.decoder))

    Logger[F].debug(s"Executing '${M.name}' Telegram method.") *>
      client
        .expect[TelegramResponse[Res]](req)
        .recoverWith { case error: InvalidMessageBodyFailure => handleUnknownEntity(M.name, request, error) }
        .flatMap(handleTelegramResponse(M, request))
  }

  private def handleUnknownEntity[I, A](method: String, input: I, error: InvalidMessageBodyFailure): F[A] =
    Logger[F].error(
      s"Received unknown Telegram entity during execution of '$method' method. \nInput data: $input. \n${error.details}"
    ) *>
      ResponseDecodingError(error.details.dropWhile(_ != '{')).raiseError[F, A]

  private def prepareRequest[Req, Res](url: Uri, method: Method[Req, Res], action: Req): Request[F] = {
    val uploads = method.attachments(action).collect { case (name, InputFile.Upload(filename, contents)) =>
      Part.fileData(name, filename, Stream.emits(contents).covary[F])
    }

    if (uploads.isEmpty) jsonRequest(url, method, action)
    else multipartRequest(url, method, action, uploads)
  }

  private def jsonRequest[Req, Res](url: Uri, method: Method[Req, Res], action: Req): Request[F] =
    Method.POST(action, url)(jsonEncoderOf(method.encoder))

  private def multipartRequest[Req, Res](url: Uri,
                                         method: Method[Req, Res],
                                         action: Req,
                                         parts: List[Part[F]]
  ): Request[F] = {
    val params =
      method
        .encoder(action)
        .asObject
        .map(
          _.toIterable
            .filterNot(kv => kv._2.isNull || kv._2.isObject)
            .map {
              case (k, j) if j.isString => k -> j.asString.get
              case (k, j)               => k -> j.toString
            }
            .toMap
        )
        .getOrElse(Map.empty)
        .map(x => Part.formData[F](x._1, x._2))

    val multipart = Multipart[F](parts.toVector ++ params.toVector)

    Method.POST(multipart, url).withHeaders(multipart.headers)
  }

  private def handleTelegramResponse[A, I, C](m: Method[I, A], input: I)(response: TelegramResponse[A]): F[A] =
    response match {
      case TelegramResponse(true, Some(result), _, _, _) => result.pure[F]

      case failed =>
        Logger[F].error(
          s"Received failed response from Telegram: $failed. Method name: ${m.name}, input data: $input"
        ) *>
          FailedMethod(m, input, failed).raiseError[F, A]
    }

  override def downloadFile(pathToFile: String): Stream[F, Byte] = {
    val uri = Uri.unsafeFromString("https://api.telegram.org/") / s"file/bot$token/" / pathToFile
    val req = Request[F](Method.GET, uri)
    client.stream(req).flatMap(_.body) //.through(_.map(_.toChar))
  }
}
