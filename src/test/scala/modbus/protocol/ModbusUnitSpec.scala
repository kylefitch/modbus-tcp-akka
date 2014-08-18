package modbus.protocol

import org.scalatest.{ WordSpec, Matchers }
import akka.util.ByteString

class ModbusUnitSpec extends WordSpec with Matchers {
  import Modbus._

  "A Modbus Protocol library" when {

    "creating a write request" should {

      "return a the correct type constructor to write a coil" in {
        val request = ModbusRequest(1, Coil(ON))
        request should equal (ModbusWriteSingleRequest(WRITE_COIL, 1, Coil(ON)))
      }

      "return a the correct type constructor to write multiple coils" in {
        val coils = Vector(Coil(OFF), Coil(OFF), Coil(OFF), Coil(OFF), Coil(OFF),
          Coil(OFF), Coil(OFF), Coil(OFF))
        val request = ModbusRequest(1, 8, coils)
        request should equal (ModbusWriteMultiRequest(WRITE_COILS, 1, 8, coils))
      }

      "return a the correct type constructor to write multiple registers" in {
        val registers = Vector(Register(1), Register(1), Register(1), Register(1), Register(1),
          Register(1), Register(1), Register(1))
        val request = ModbusRequest(1, 8, registers)
        request should equal (ModbusWriteMultiRequest(WRITE_REGISTERS, 1, 8, registers))
      }

    }

  }

}
