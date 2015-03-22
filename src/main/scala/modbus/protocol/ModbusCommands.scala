package modbus.protocol

object ModbusCommands {
  import ModbusTypes._
  import ModbusCodes.FunctionCodes._

  sealed trait ModbusCommand {
    val functionCode: Byte
  }

  case class ReadCoils(startAddress: Int, quantity: Int) extends ModbusCommand {
    val functionCode = readCoilsCode
  }

  case class ReadDescreteInputs(startAddress: Int, quantity: Int) extends ModbusCommand {
    val functionCode = readDiscreteInputsCode
  }

  case class ReadRegisters(startAddress: Int, quantity: Int) extends ModbusCommand {
    val functionCode = readRegistersCode
  }

  case class ReadInputRegisters(startAddress: Int, quantity: Int) extends ModbusCommand {
    val functionCode = readInputRegistersCode
  }

  case class WriteCoil[A](address: Int, value: ModbusType[A]) extends ModbusCommand {
    val functionCode = writeCoilCode
  }

  case class WriteRegister[A](address: Int, value: ModbusType[A]) extends ModbusCommand {
    val functionCode = writeRegisterCode
  }

  case class WriteCoils[A](startAddress: Int, values: Seq[ModbusType[A]]) extends ModbusCommand {
    val functionCode = writeCoilsCode
  }

  case class WriteRegisters[A](startAddress: Int, values: Seq[ModbusType[A]]) extends ModbusCommand {
    val functionCode = writeRegistersCode
  }

}