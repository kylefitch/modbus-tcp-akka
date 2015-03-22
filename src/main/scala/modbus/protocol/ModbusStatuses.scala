package modbus.protocol

object ModbusStatuses {
  import ModbusTypes._

  sealed trait ModbusStatus

  case class CoilStatus(functionCode: Byte, value: Coil) extends ModbusStatus

  case class CoilsStatus(functionCode: Byte, values: Seq[Coil]) extends ModbusStatus

  case class DiscreteInputsStatus(functionCode: Byte, values: Seq[DiscreteInput]) extends ModbusStatus

  case class RegisterStatus(functionCode: Byte, value: Register) extends ModbusStatus

  case class RegistersStatus(functionCode: Byte, values: Seq[Register]) extends ModbusStatus

  case class InputRegistersStatus(functionCode: Byte, values: Seq[InputRegister]) extends ModbusStatus

}
