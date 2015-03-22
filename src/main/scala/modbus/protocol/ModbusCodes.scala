package modbus.protocol

object ModbusCodes {

  object FunctionCodes {
    final val readCoilsCode: Byte = 1
    final val readDiscreteInputsCode: Byte = 2
    final val readRegistersCode: Byte = 3
    final val readInputRegistersCode: Byte = 4
    final val writeCoilCode: Byte = 5
    final val writeRegisterCode: Byte = 6
    final val writeCoilsCode: Byte = 15
    final val writeRegistersCode: Byte = 16
  }

  object ErrorCodes {

  }
}
