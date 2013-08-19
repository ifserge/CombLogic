import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 19.08.13
 * Time: 11:45
 */
class WeakReductorTest extends FunSuite  {
  test("empty -> empty"){
    val result = WeakReductor(EmptyTerm())
    assert(result === EmptyTerm())
  }
  test("atom -> atom"){
    val result = WeakReductor(Atom('x'))
    assert(result === Atom('x'))
  }
  test("Ix -> x"){
    val result = WeakReductor(Application(Atom('I'), Atom('x')))
    assert(result === Atom('x'))
  }
  test("Kxy -> x"){
    val result = WeakReductor(Application(Application(Atom('K'), Atom('x')), Atom('y')))
    assert(result === Atom('x'))
  }
  test("Sxyz -> xz(yz)"){
    val result = WeakReductor(CombParser.parse("Sxyz", EmptyTerm())._2)
    val parsed = CombParser.parse("xz(yz)", EmptyTerm())._2
    assert(result === parsed)
  }
  test("KKKKx -> K)"){
    val result = WeakReductor(CombParser.parse("KKKKx", EmptyTerm())._2)
    val parsed = CombParser.parse("K", EmptyTerm())._2
    assert(result === parsed)
  }
  test("SSSSx -> K)"){
    val result = WeakReductor(CombParser.parse("SSSSx", EmptyTerm())._2)
    val parsed = CombParser.parse("Sx(SSx)", EmptyTerm())._2
    assert(result === parsed)
  }
}
