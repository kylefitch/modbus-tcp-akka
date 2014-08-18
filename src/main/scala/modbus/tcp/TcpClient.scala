package modbus.tcp

import akka.actor._
import java.net.InetSocketAddress
import akka.io.{IO, Tcp}
import akka.util.ByteString
import TcpClient._
import scala.collection.immutable.Queue

class TcpClient(remote: InetSocketAddress, client: ActorRef) extends Actor
  with LoggingFSM[State, StateData] {
  import Tcp._

  implicit val system = context.system

  final val maxFailures = 10

  startWith(DisconnectedState, StateData(Queue[ByteString](), null, 0))

  when(DisconnectedState) {

    case Event(c: Connected, data: StateData) =>
      val newConn = sender()
      newConn ! Register(self)
      goto(ConnectedState) using data.copy(conn = newConn)

    case Event(CommandFailed(_: Connect), data: StateData) =>
      log.warning(s"Connecting to [$remote] failed.")
      stay using data

    case Event(s: Send, data: StateData) =>
      connect()
      val newBuffer = buffer(data.buffer, s.bytes)
      stay using data.copy(buffer = newBuffer)

  }

  when(ConnectedState) {

    case Event(s: Send, data: StateData) =>
      val newBuffer = buffer(data.buffer, s.bytes)
      if(data.buffer.isEmpty) send(s.bytes, data.conn)
      stay using data.copy(buffer = newBuffer)

    case Event(r: Received, data: StateData) =>
      client ! r.data
      if(data.buffer.isEmpty) close(data.conn)
      stay using data

    case Event(Ack, data: StateData) =>
      val newBuffer = acknowledge(data.buffer, data.conn)
      if(newBuffer.nonEmpty) send(newBuffer.head, data.conn)
      stay using data.copy(buffer = newBuffer)

    case Event(CommandFailed(_: Write), data: StateData) =>
      val newFailures = addWriteFailure(data.failures)
      retry(data.buffer, data.conn, newFailures)
      stay using data.copy(failures = newFailures)

    case Event(_: ConnectionClosed, data: StateData) =>
      goto(DisconnectedState) using data.copy(conn = null, failures = 0)

  }

  onTransition {

    case DisconnectedState -> ConnectedState =>
      if(nextStateData.buffer.nonEmpty) send(nextStateData.buffer.head, nextStateData.conn)
      log.debug(s"TCP Client Connected to [$remote]")

    case ConnectedState -> DisconnectedState => log.debug(s"TCP Client Disconnected from [$remote]")

  }

  initialize()

  def connect(): Unit = IO(Tcp) ! Connect(remote)

  def close(conn: ActorRef): Unit = {
    conn ! Close
    log.debug(s"Buffer empty. Disconnecting from [$remote].")
  }

  def send(bs: ByteString, conn: ActorRef): Unit = conn ! Write(bs, Ack)

  def buffer(b: Queue[ByteString], bs: ByteString): Queue[ByteString] = b :+ bs

  def acknowledge(buffer: Queue[ByteString], conn: ActorRef): Queue[ByteString] = {
    require(buffer.nonEmpty, "Ack but buffer empty.")
    buffer drop 1
  }

  def addWriteFailure(failures: Int): Int = failures + 1

  def retry(buffer: Queue[ByteString], conn: ActorRef, failures: Int): Unit = {
    log.warning(s"Write error. Failures: $failures")
    if (failures < maxFailures) send(buffer.head, conn) else conn ! Close
  }

}

object TcpClient {

  sealed trait State
  case object ConnectedState extends State
  case object ConnectingState extends State
  case object DisconnectedState extends State

  case class StateData(buffer: Queue[ByteString], conn: ActorRef, failures: Int)

  case object Ack extends Tcp.Event

  case class Send(bytes: ByteString)
  case object Ready

  def props(remote: InetSocketAddress, replies: ActorRef) =
    Props(classOf[TcpClient], remote, replies)
}
