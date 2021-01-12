import org.scalatest.funsuite.AnyFunSuite

import cynthia.utils.Str

class StrTest extends AnyFunSuite {
  test("append a value to an Str object") {
    val str = Str("foo")
    assert(str.toString == "foo")
    val appendedStr = str << "bar"
    assert(appendedStr.toString == "foobar")
  }
} 
