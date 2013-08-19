/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 16.08.13
 * Time: 17:55
 */

import org.scalatest.FunSuite

class AbstractTransformerTest extends FunSuite {
  test("empty -> empty"){
    val result = AbstractTransformer(EmptyTerm())
    assert(result === EmptyTerm())
  }
  test("atom -> atom"){
    val result = AbstractTransformer(Atom('x'))
    assert(result === Atom('x'))
  }
  test("app -> app"){
    val result = AbstractTransformer(Application(Atom('I'), Atom('x')))
    assert(result === Application(Atom('I'), Atom('x')))
  }
  test("[x].x -> I") {
    val result = AbstractTransformer(Abstraction(Atom('x'), Atom('x')))
    assert(result === Atom('I'))
  }
  test("[x].t -> Kt") {
    val result = AbstractTransformer(Abstraction(Atom('x'), Atom('t')))
    assert(result === Application(Atom('K'), Atom('t')))
  }
  test("[x].Ux -> U") {
    val U = Application(Atom('S'), Application(Atom('y'), Atom('z')))
    val Ux = Application(U, Atom('x'))
    val result = AbstractTransformer(Abstraction(Atom('x'), Ux))
    assert(result === U)
  }
  test("[x].(yx)(xy) -> Sy(SI(Ky))") {
    val U = Application(Atom('y'), Atom('x'))
    val V = Application(Atom('x'), Atom('y'))
    val result = AbstractTransformer(Abstraction(Atom('x'), Application(U,V)))
    val expected = CombParser.parse("Sy(SI(Ky))", EmptyTerm())._2
    assert(result === expected)
  }
  test("[x,y,z].xz(yz) -> S") {
    val result = AbstractTransformer(CombParser.parse("[x,y,z].xz(yz)", EmptyTerm())._2)
    assert(result === Atom('S'))
  }
  test("[x,y].yx -> S((S(KS))(KI))K") {
    val result = AbstractTransformer(CombParser.parse("[x,y].yx", EmptyTerm())._2)
    assert(result === CombParser.parse("S((S(KS))(KI))K", EmptyTerm())._2)
  }
}
