package modbus.protocol

import java.nio.ByteOrder
import akka.util.ByteString

object Modbus {
  import ModbusTypes._
  import ModbusCommands._
  implicit val byteOrder = ByteOrder.BIG_ENDIAN

  final val ERROR_CODE_OFFSET: Byte = 0x80.toByte

  case class ModbusHeader(transactionID: Int, protocolID: Int = 0, remainingLength: Int, unitID: Byte = 0xFF.toByte)

  case class ModbusRequest(header: ModbusHeader, command: ModbusCommand)
  // case class ModbusResponse(header: ModbusHeader, command: ModbusCommand)

  sealed trait ModbusResponse
  case class ModbusReadResponse[A](fc: Byte, sa: Int, q: Int, vs: Vector[ModbusValue[A]]) extends ModbusResponse
  case class ModbusWriteSingleResponse[A](fc: Byte, a: Int, v: ModbusValue[A]) extends ModbusResponse
  case class ModbusWriteMultiResponse(fc: Byte, a: Int, q: Int) extends ModbusResponse
  case class ModbusErrorResponse(fc: Byte = 0, errorCode: Byte = 4) extends ModbusResponse

  object ModbusResponse {

    def apply[A](sa: Int, q: Int, vs: Vector[ModbusValue[A]]): ModbusResponse = vs.head match {
      case Coil(_) => ModbusReadResponse (READ_COILS, sa, q, vs)
      case Register(_) => ModbusReadResponse (READ_REGISTERS, sa, q, vs)
    }

    def apply[A](a: Int, v: ModbusValue[A]): ModbusResponse = v match {
      case Coil(_) => ModbusWriteSingleResponse(WRITE_COIL, a, v)
      case Register(_) => ModbusWriteSingleResponse(WRITE_REGISTER, a, v)
    }

    def apply(fc: Byte, a: Int, q: Int): ModbusResponse = ModbusWriteMultiResponse(fc, a, q)

  }

  case object UnrecoverableModbusException extends RuntimeException
  case object FunctionCodeException extends RuntimeException
  case object DataAddressException extends RuntimeException
  case object DataValueException extends RuntimeException
  case object GeneralModbusException extends RuntimeException


  type ValueEncoder[A] = Vector[ModbusValue[A]] => ByteString
  type ValueDecoder[A] = ByteString => Vector[ModbusValue[A]]

  def encodeValues[A](values: Vector[ModbusValue[A]], encoder: ValueEncoder[A]): ByteString = encoder (values)

  def decodeValues[A](pdu: ByteString, decoder: ValueDecoder[A]): Vector[ModbusValue[A]] = decoder (pdu)

  val encodeCoils = { coils: Vector[ModbusValue[Boolean]] =>
    val bsb = ByteString.newBuilder
    coils.grouped(8)
      .toList
      .map { coilGroup =>
      coilGroup.zipWithIndex
        .foldRight(0) { (coil: (ModbusValue[Boolean], Int), byte: Int) =>
          coil match {
            case (Coil(v), i) => if (v) 1 << i else 0 + byte
            case _ => byte
          }
        }
      }
      .map { byte => bsb.putByte(byte.toByte) }
    bsb.result()
  }

  val encodeCoil = { coils: Vector[ModbusValue[Boolean]] =>
    val bsb = ByteString.newBuilder
    coils.head match {
      case Coil(v) => bsb.putShort(if (v) 0xFF00 else 0)
      case _ =>
    }
    bsb.result()
  }

  val encodeRegisters = { registers: Vector[ModbusValue[Int]] =>
    val bsb = ByteString.newBuilder
    registers map {
      case Register(v) => bsb.putShort(v)
      case _ =>
    }
    bsb.result()
  }

  val decodeCoils = { bytes: ByteString =>
    bytes.flatMap { byte =>
      for(offset <- 0 to 7) yield if(((byte >>> offset) & 1) == 1) Coil(true) else Coil(false) }
      .toVector
  }

  val decodeCoil = { bytes: ByteString =>
    val value = bytes.iterator.getShort & 0xFFFF
    if (value == 0) Vector(Coil(false))
    else if (value == 0xFF00) Vector(Coil(true))
    else throw DataValueException
  }

  val decodeRegisters = { bytes: ByteString =>
    bytes.sliding(2, 2)
      .map { bytePair => bytePair.iterator.getShort }
      .zipWithIndex
      .map { case (v, i) => Register(v) }
      .toVector
  }


  type ByteCounter[A] = Vector[ModbusValue[A]] => Byte

  def getByteCount[A](vs: Vector[ModbusValue[A]], bc: ByteCounter[A]): Byte = bc(vs)

  val coilByteCount = { coils: Vector[ModbusValue[Boolean]] =>
    if(coils.size % 8 != 0) ((coils.size / 8) + 1).toByte
    else (8 / coils.size).toByte
  }

  val registerByteCount = { registers: Vector[ModbusValue[Int]] =>
    (registers.size * 2).toByte
  }


  def decodeIndication(pdu: ByteString): ModbusRequest = {

    def decodeReadRequest(fcode: Byte, pdu: ByteString): ModbusRequest = {
      val i = pdu.iterator
      val address = decodeAddress(i.getShort)
      val numCoils = i.getShort
      ModbusRequest(fcode, address, numCoils)
    }

    def decodeWriteSingleRequest[A](fcode: Byte, pdu: ByteString, vd: ValueDecoder[A]): ModbusRequest = {
      val i = pdu.iterator
      val address = decodeAddress(i.getShort)
      val value = decodeValues(i.toByteString, vd)
      ModbusRequest(address, value.head)
    }

    def decodeWriteMultipleRequest[A](fcode: Byte, pdu: ByteString, vd: ValueDecoder[A]): ModbusRequest = {
      val i = pdu.iterator
      val startAddress = decodeAddress (i.getShort)
      val quantity = i.getShort
      val byteCount = i.getByte
      val values = decodeValues (i.toByteString, vd)
      ModbusRequest(fcode, startAddress, values)
    }

    val i = pdu.iterator
    val functionCode = i.getByte
    functionCode match {
      case READ_COILS | READ_REGISTERS => decodeReadRequest(functionCode, i.toByteString)
      case WRITE_COIL => decodeWriteSingleRequest(functionCode, i.toByteString, decodeCoil)
      case WRITE_REGISTER => decodeWriteSingleRequest(functionCode, i.toByteString, decodeRegisters)
      case WRITE_COILS => decodeWriteMultipleRequest(functionCode, i.toByteString, decodeCoils)
      case WRITE_REGISTERS => decodeWriteMultipleRequest(functionCode, i.toByteString, decodeRegisters)
      case _ => throw FunctionCodeException
    }

  }

  def decodeConfirmation(pdu: ByteString): ModbusResponse = {

    def decodeReadResponse[A](fcode: Byte, pdu: ByteString, vd: ValueDecoder[A]): ModbusResponse = {
      val i = pdu.iterator
      val byteCount = i.getByte
      val values = decodeValues(i.toByteString, vd)
      ModbusResponse(fcode, values.size, values)
    }

    def decodeWriteSingleResponse(fcode: Byte, pdu: ByteString): ModbusResponse = {
      val i = pdu.iterator
      val address = decodeAddress(i.getShort)
      val value = decodeValues(i.toByteString, decodeCoils)
      ModbusResponse(address, value.head)
    }

    def decodeWriteMultipleResponse(fcode: Byte, pdu: ByteString): ModbusResponse = {
      val i = pdu.iterator
      val address = decodeAddress(i.getShort)
      val quantity = i.getShort
      ModbusResponse(fcode, address, quantity)
    }

    val i = pdu.iterator
    val fcode = i.getByte
    fcode match {
      case READ_COILS => decodeReadResponse(fcode, i.toByteString, decodeCoils)
      case READ_REGISTERS => decodeReadResponse(fcode, i.toByteString, decodeRegisters)
      case WRITE_COIL | WRITE_REGISTER => decodeWriteSingleResponse(fcode, i.toByteString)
      case WRITE_COILS | WRITE_REGISTERS => decodeWriteMultipleResponse(fcode, i.toByteString)
      case _ => ModbusErrorResponse(fcode, 4)
    }

  }

  def encodeRequest[A](request: ModbusRequest): ByteString = {

    def encodeWriteSingleRequest(r: ModbusWriteSingleRequest[A]): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte(r.fc)
      bsb.putShort(encodeAddress(r.a))
      bsb ++= ( r.v match {
        case Coil(_) => encodeValues(Vector(r.v), encodeCoil)
        case Register(_) => encodeValues(Vector(r.v), encodeRegisters)
      })
      bsb.result()
    }

    def encodeWriteMultipleRequest(r: ModbusWriteMultiRequest[A]): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte(r.fc)
      bsb.putShort(encodeAddress(r.sa))
      bsb.putShort(r.q)
      r.vs.head match {
        case Coil(_) => bsb.putByte(getByteCount(r.vs, coilByteCount))
        case Register(_) => bsb.putByte(getByteCount(r.vs, registerByteCount))
      }
      bsb ++= ( r.vs.head match {
        case Coil(_) => encodeValues (r.vs, encodeCoils)
        case Register(_) => encodeValues (r.vs, encodeRegisters)
      })
      bsb.result()
    }

    def encodeReadRequest(r: ModbusReadRequest): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte(r.fc)
      bsb.putShort(encodeAddress(r.sa))
      bsb.putShort(r.q)
      bsb.result()
    }

    request match {
      case wsr: ModbusWriteSingleRequest[A] => encodeWriteSingleRequest(wsr)
      case wmr: ModbusWriteMultiRequest[A] => encodeWriteMultipleRequest(wmr)
      case rr: ModbusReadRequest => encodeReadRequest(rr)
    }
  }

  def encodeResponse[A](response: ModbusResponse): ByteString = {

    def encodeWriteSingleResponse(r: ModbusWriteSingleResponse[A]): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte(r.fc)
      bsb.putShort(encodeAddress(r.a))
      bsb ++= ( r.v match {
        case Coil(_) => encodeValues(Vector(r.v), encodeCoil)
        case Register(_) => encodeValues(Vector(r.v), encodeRegisters)
      })
      bsb.result()
    }

    def encodeWriteMultipleResponse(r: ModbusWriteMultiResponse): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte(r.fc)
      bsb.putShort(encodeAddress(r.a))
      bsb.putShort(r.q)
      bsb.result()
    }

    def encodeReadResponse(r: ModbusReadResponse[A]): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte(r.fc)
      r.vs.head match {
        case Coil(_) => bsb.putByte(getByteCount(r.vs, coilByteCount))
        case Register(_) => bsb.putByte(getByteCount(r.vs, registerByteCount))
      }
      bsb ++= (r.vs.head match {
        case Coil(_) => encodeValues (r.vs, encodeCoils)
        case Register(_) => encodeValues (r.vs, encodeRegisters)
      })
      bsb.result()
    }

    def encodeErrorResponse(r: ModbusErrorResponse): ByteString = {
      val bsb = ByteString.newBuilder
      bsb.putByte((r.fc + ERROR_CODE_OFFSET).toByte)
      bsb.putByte(r.errorCode)
      bsb.result()
    }

    response match {
      case r: ModbusWriteSingleResponse[A] => encodeWriteSingleResponse(r)
      case r: ModbusWriteMultiResponse => encodeWriteMultipleResponse(r)
      case r: ModbusReadResponse[A] => encodeReadResponse(r)
      case r: ModbusErrorResponse => encodeErrorResponse(r)
    }

  }

  def encodeHeader(header: ModbusHeader): ByteString = {
    val builder = ByteString.newBuilder
    builder.putShort(header.transactionID)
    builder.putShort(header.protocolID)
    builder.putShort(header.remainingLength)
    builder.putByte(header.unitID)
    builder.result()
  }

  def decodeHeader(head: ByteString): ModbusHeader = {
    val i = head.iterator
    val transID = i.getShort & 0xFFFF
    val protocolID = i.getShort & 0xFFFF
    val rLength = i.getShort & 0xFFFF
    val unitID = i.getByte
    ModbusHeader(transID, protocolID, rLength, unitID)
  }

  def encodeAddress(address: Int): Int = (address - 1) + ADDRESS_OFFSET

  def decodeAddress(address: Int): Int = (address + 1) - ADDRESS_OFFSET

}
