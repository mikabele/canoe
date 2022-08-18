package canoe

import cats.syntax.all._
import canoe.models.{CallbackQuery, Messageable}
import Messageable._
import canoe.models.messages.{DocumentMessage, TelegramMessage, TextMessage}
package object syntax extends Contents with Expects {

  type Expect[A] = PartialFunction[Messageable, A]

  def callback(associatedMessage: Option[TelegramMessage]): Expect[CallbackQuery] = {
    case Messageable.MyCallbackQuery(query)
      if query.message.flatMap(msg => associatedMessage.map(_.messageId === msg.messageId)).getOrElse(false) =>
      query
  }

  def command(cmd: String): Expect[TextMessage] = {
    case Messageable.MyTelegramMessage(message: TextMessage) if message.text === ("/" + cmd) => message
  }

  def containing(str: String): Expect[TextMessage] = {
    case Messageable.MyTelegramMessage(message: TextMessage) if message.text === str => message
  }

  val textMessage: Expect[TextMessage] = {
    case Messageable.MyTelegramMessage(message: TextMessage) => message
  }
  val documentMessage: Expect[DocumentMessage] = {
    case MyTelegramMessage(message: DocumentMessage) => message
  }

  implicit def partialFunctionOps[A, B](original: PartialFunction[A, B]): PartialFunctionOps[A, B] =
    new PartialFunctionOps[A, B](original)

  implicit def expectTelegramMessageOps(original: Expect[TelegramMessage]): ExpectTelegramMessageOps =
    new ExpectTelegramMessageOps(original)

  implicit def expectTextMessageOps(textMessage: Expect[TextMessage]): ExpectTextMessageOps =
    new ExpectTextMessageOps(textMessage)

  implicit def methodOps[A](a: A): MethodSyntax[A] = new MethodSyntax[A](a)
}
