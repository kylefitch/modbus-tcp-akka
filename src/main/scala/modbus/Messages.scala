package modbus

import modbus.protocol.Modbus.ModbusValue

object Messages {

  sealed trait ModbusMessage
  case class WriteCoil(address: Int, value: ModbusValue[Boolean]) extends ModbusMessage
  case class WriteRegister(address: Int, value: ModbusValue[Int]) extends ModbusMessage
  case class WriteCoils(startAddress: Int, quantity: Int, values: Vector[ModbusValue[Boolean]]) extends ModbusMessage
  case class WriteRegisters(startAddress: Int, quantity: Int, values: Vector[ModbusValue[Int]]) extends ModbusMessage
  case class ReadCoils(startAddress: Int, quantity: Int) extends ModbusMessage
  case class ReadRegisters(startAddress: Int, quantity: Int) extends ModbusMessage

}
