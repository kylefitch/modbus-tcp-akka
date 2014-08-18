package modbus.server

import akka.actor.{ Props, ActorRef, ActorLogging, Actor }
import akka.io.Tcp.{ Write, PeerClosed, Received }
import akka.util.ByteString
import modbus.protocol.Modbus
import scala.util.Try

class ModbusHandler(producer: ActorRef) extends Actor with ActorLogging {
  import Modbus._

  def receive = {
    case Received(data) =>
    case PeerClosed => context stop self
  }

  private def decodeMessage(bytes: ByteString): (ModbusHeader, ModbusRequest) = {
    val (head, pdu) = bytes.splitAt(7)
    val message = for {
      header <- Try(decodeHeader(head))
      request <- Try(decodeIndication(pdu))
    } yield (header, request)
    message getOrElse (throw new RuntimeException)
  }

  private def encodeMessage(header: ModbusHeader, response: ModbusResponse): ByteString = {
    val message = for {
      head <- Try(encodeHeader(header))
      pdu <- Try(encodeResponse(response))
    } yield head ++ pdu
    message getOrElse (throw new RuntimeException)
  }

}

object ModbusHandler {
  def props(producer: ActorRef) = Props(classOf[ModbusHandler], producer)
}
