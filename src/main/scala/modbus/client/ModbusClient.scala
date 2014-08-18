package modbus.client

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import java.net.InetSocketAddress
import akka.util.ByteString
import java.time.Instant
import modbus.protocol.Modbus
import modbus.tcp.TcpClient

import scala.concurrent.duration._
import scala.util.Try

class ModbusClient(tcpClient: ActorRef, con: ActorRef) extends Actor with ActorLogging {
  import Modbus._
  import ModbusClient._
  import TcpClient.Send

  implicit val ec = context.dispatcher

  val MAX_RETRIES = 5
  val TIMEOUT_MILLIS = 300
  val TIMEOUT = TIMEOUT_MILLIS.millis
  val RESET_TIME = 10.minutes
  val MAX_TRANS_ID = 0xFFFF

  var transactions = Map[Int, Transaction]()
  var modbusTransactionID = 0

  val ticker = context.system.scheduler.schedule(TIMEOUT, TIMEOUT, self, Tick)

  val counterReset = context.system.scheduler.schedule(RESET_TIME, RESET_TIME, self, ResetCounter)

  def receive = {
    case m: ModbusRequest => handleRequest(m)
    case b: ByteString => handleConfirmation(b)
    case Tick => handleTick()
    case ResetCounter => handleResetCounter()
  }

  private def handleRequest(r: ModbusRequest) = {
    val transaction = buildTransaction(r)
    storeModbusTransaction(transaction)
    sendMessage(transaction)
  }

  private def handleConfirmation(bytes: ByteString) = {
    val (header, response) = decodeMessage(bytes)
    response match {
      case rr: ModbusReadResponse => handleReadResponse(header, rr)
      case wsr: ModbusWriteSingleResponse => handleWriteResponse(header, wsr)
      case wmr: ModbusWriteMultiResponse  => handleWriteResponse(header, wmr)
      case er: ModbusErrorResponse => handleErrorResponse(header, er)
    }
  }

  private def handleTick() = {
    val now = Instant.now()
    transactions foreach { case (i, t) =>
      if (now isAfter t.time.plusMillis(TIMEOUT_MILLIS)) {
        log.error(s"Transaction ID: $i timed out. Dropped.")
        dropModbusTransaction(i)
      }
    }
  }

  private def handleResetCounter() = modbusTransactionID = 0

  private def handleReadResponse(header: ModbusHeader, response: ReadResponse) = {
    transactions get header.transactionID match {
      case Some(t) =>
        con ! response
        dropModbusTransaction(t.id)
      case None => log.error("No transaction in map.")
    }
  }

  private def handleWriteResponse(header: ModbusHeader, response: ModbusResponse) = {
    transactions get header.transactionID match {
      case Some(t) => dropModbusTransaction(t.id)
      case None => log.error("No transaction in map.")
    }
  }

  private def handleErrorResponse(header: ModbusHeader, response: ErrorResponse) = {
    transactions get header.transactionID match {
      case Some(t) =>
        if(t.retries < MAX_RETRIES) {
          updateRetries(t)
          sendMessage(t)
        } else { val id = t.id
          dropModbusTransaction(id)
          log.error(s"Transaction ID: $id reached max retries. Dropped.")
        }
      case None => log.error("No transaction in map.")
    }
  }

  private def sendMessage(transaction: Transaction) = {
    tcpClient ! Send(encodeMessage(transaction))
  }

  private def buildTransaction(request: ModbusRequest): Transaction = {
    val transID = nextTransactionID()
    val header = buildHeader(transID, request)
    Transaction(transID, header, request, 0, Instant.now())
  }

  private def buildHeader(transactionID: Int, request: ModbusRequest): ModbusHeader = {
    val rLength = getRemainingLength(request)
    ModbusHeader(transactionID = transactionID, remainingLength = rLength)
  }

  private def nextTransactionID(): Int = {
    modbusTransactionID += 1
    modbusTransactionID
  }

  private def storeModbusTransaction(transaction: Transaction) =
    transactions += (transaction.id -> transaction)

  private def dropModbusTransaction(id: Int) = transactions = transactions drop id

  private def updateRetries(transaction: Transaction) = { transactions =
    transactions.updated(transaction.id, transaction.copy(retries = transaction.retries + 1))
  }

  private def encodeMessage(transaction: Transaction): ByteString = {
    val message = for {
      header <- Try(encodeHeader(transaction.header))
      pdu <- Try(encodeRequest(transaction.request))
    } yield header ++ pdu
    message getOrElse (throw new RuntimeException)
  }

  private def decodeMessage(bytes: ByteString): (ModbusHeader, ModbusResponse) = {
    val (head, pdu) = bytes.splitAt(7)
    val message = for {
      header <- Try(decodeHeader(head))
      response <- Try(decodeConfirmation(pdu))
    } yield (header, response)
    message getOrElse (throw new RuntimeException)
  }

  private def getRemainingLength(request: ModbusRequest): Int = request match {
    case ModbusWriteSingleRequest(_, _) => 6
    case ModbusWriteMultiRequest(_, _, _, bc, _) => 7
    case ModbusReadRequest(_, _, _) => 6
  }

}

object ModbusClient {
  import Modbus._

  val REMOTE_SERVER_PORT = 502
  val REMOTE_HOST = "192.168.10.211"
  val remote = new InetSocketAddress(REMOTE_HOST, REMOTE_SERVER_PORT)

  case object Tick
  case object ResetCounter

  case class Transaction(id: Int, header: ModbusHeader, request: ModbusRequest, retries: Int,
                         time: Instant)
  case class Retry(transID: Int)

  def props(tcpClient: ActorRef) = Props(classOf[ModbusClient], tcpClient)
}
