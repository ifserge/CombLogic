/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 16.08.13
 * Time: 13:58
 */

import org.scalatest.FunSuite

class CombParserTest extends FunSuite {
  test("empty") {
    val result = CombParser.parse("", EmptyTerm())
    assert(result === ("",EmptyTerm()))
  }
  test("SKI") {
    val result = CombParser.parse("SKI", EmptyTerm())
    assert(result === ("",Application(Application(Atom('S'), Atom('K')), Atom('I'))))
  }
  test("[x].Kx") {
    val result = CombParser.parse("[x].Kx", EmptyTerm())
    assert(result === ("",Abstraction(Atom('x'), Application(Atom('K'), Atom('x')))))
  }
  test("[x,y].Kxy") {
    val result = CombParser.parse("[x,y].Kxy", EmptyTerm())
    assert(result === ("",Abstraction(Atom('x'), Abstraction(Atom('y'), Application(Application(Atom('K'), Atom('x')), Atom('y'))))))
  }
}
