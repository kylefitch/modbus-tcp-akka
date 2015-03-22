package modbus.protocol

object ModbusTypes {

  sealed trait ModbusType[A]

  case class Coil(c: Boolean) extends ModbusType[Boolean]

  case class DiscreteInput(di: Boolean) extends ModbusType[Boolean]

  case class Register(r: Int) extends ModbusType[Int]

  case class InputRegister(ir: Int) extends ModbusType[Int]

}