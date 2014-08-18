package modbus.server

import akka.actor.{ Props, ActorRef, ActorLogging, Actor }
import akka.io.Tcp.{Write, PeerClosed, Received}
import akka.util.ByteString
import modbus.protocol.Modbus
import scala.util.Try

class ModbusHandler(producer: ActorRef) extends Actor with ActorLogging {
  import Modbus._

  def receive = {
    case Received(data) =>
      val (header, request) = decodeMessage(data)
      sender ! Write(response(header, request))
      producer ! getValues(request)
    case PeerClosed => context stop self
  }

  private def response(header: ModbusHeader, request: ModbusRequest): ByteString = {
    val mbresponse = createResponse(request)
    val responseHeader = buildResponseHeader(header, mbresponse)
    encodeMessage(responseHeader, mbresponse)
  }

  def buildResponseHeader(header: ModbusHeader, response: ModbusResponse): ModbusHeader = {
    val rLength = response match {
      case ReadResponse(_, bc, _) => 3 + bc
      case WriteSingleResponse(_, _) => 6
      case WriteMultipleResponse(_, _, _) => 6
      case ErrorResponse(_, _) => 3
    }
    ModbusHeader(header.transactionID, header.protocolID, rLength, header.unitID)
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

  def getValues(request: ModbusRequest): ModbusIO = request match {
    case WriteSingleRequest(_, v) => ModbusWrite(Vector(v))
    case WriteMultipleRequest(_, _, _, _, vs) => ModbusWrite(vs)
    case _ => ModbusWrite(Vector())
  }

}

object ModbusHandler {
  def props(producer: ActorRef) = Props(classOf[ModbusHandler], producer)
}
