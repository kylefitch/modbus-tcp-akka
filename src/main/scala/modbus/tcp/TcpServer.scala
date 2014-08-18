package modbus.tcp

import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.io.{ IO, Tcp }
import java.net.InetSocketAddress
import modbus.server.ModbusHandler

class TcpServer(producer: ActorRef, socket: InetSocketAddress) extends Actor with ActorLogging {
  import TcpServer._
  import Tcp._

  implicit val system = context.system

  IO(Tcp) ! Bind(self, socket)

  def receive = {
    case b @ Bound(localAddress) => log.info(s"Server started at [$LOCAL_HOST] on port [$SERVER_PORT].")

    case CommandFailed(_: Bind) =>
      log.error(s"Server failed to start at [$LOCAL_HOST] on port [$SERVER_PORT].")
      context stop self

    case c @ Connected(remote, local) =>
      log.debug("Server connection opened.")
      val handler = system.actorOf(ModbusHandler.props(producer))
      val connection = sender()
      connection ! Register(handler)
  }

}

object TcpServer {
  def LOCAL_HOST = "192.168.10.210"
  def SERVER_PORT = 5020

  def props(producer: ActorRef, address: String, port: Int) =
    Props(classOf[TcpServer], producer, new InetSocketAddress(address, port))
}
