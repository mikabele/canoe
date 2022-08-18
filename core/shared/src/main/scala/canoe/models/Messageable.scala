package canoe.models

import canoe.models.messages.TelegramMessage
import cats.syntax.all._

sealed trait Messageable {
  def getMessage: Option[TelegramMessage]

}

object Messageable {

  final case class MyTelegramMessage(
                                      message: TelegramMessage
                                    ) extends Messageable {
    override def getMessage: Option[TelegramMessage] = message.some
  }

  final case class MyCallbackQuery(
                                    query: CallbackQuery
                                  ) extends Messageable {
    override def getMessage: Option[TelegramMessage] = query.message
  }
}

