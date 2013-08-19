/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 22.07.13
 * Time: 13:09
 */
trait Term
case class EmptyTerm() extends Term
case class Atom(c: Char) extends Term
case class Application(terml: Term, termr: Term) extends Term
case class Abstraction(binds: Atom, body: Term) extends Term

object MetaTerm
{
  def lgh(t: Term): Int =
    t match {
      case EmptyTerm() => 0
      case Atom(_) => 1
      case Application(t1, t2) => lgh(t1) + lgh(t2)
      case Abstraction(lst, t1) => lgh(t1)
    }
  def depth(t: Term): Int =
    t match {
      case EmptyTerm() => 0
      case Atom(_) => 1
      case Application(t1, t2) => 1 + List(depth(t1), depth(t2)).max
      case Abstraction(lst, t1) => depth(t1)
    }
  def atoms(t: Term): Set[Atom] =
    t match {
      case EmptyTerm() => Set.empty
      case Atom(x) => Set(Atom(x))
      case Application(t1, t2) => atoms(t1) ++ atoms(t2)
      case Abstraction(lst, t1) => atoms(t1)
    }
}

object TermPrinter
{
  def apply(t: Term) : String =
    t match {
      case EmptyTerm() => ""
      case Atom(c) => "" + c
      case Application(a, b) => "(" + TermPrinter(a) + TermPrinter(b) + ")"
      case Abstraction(a, b) => "([" + TermPrinter(a) + "]." + TermPrinter(b) + ")"
    }
}

