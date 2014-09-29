package recfun

import org.scalatest.FunSuite

/** @author zenind */
class Main$TestHigherOrder extends FunSuite {

  import Main.sum

  test("test sum of simple integers") {
    assert(sum(x => x, 1, 5) === 15)
  }

}
