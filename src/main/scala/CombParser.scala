import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 22.07.13
 * Time: 12:26
  */
object AbstractionParser {
  def correctAbstraction(atoms: List[Atom], t: Term): Term =
    atoms.length match {
      case 0 => EmptyTerm()
      case 1 => Abstraction(atoms.head, t)
      case k => Abstraction(atoms.head, correctAbstraction(atoms.tail, t))
    }
  def parse(s: String, atoms: List[Atom]) : (String, Term) =
    if (s.isEmpty) (s, correctAbstraction(atoms.reverse, EmptyTerm()))
    else {
      s.charAt(0) match {
        case ']' => {
          val res = CombParser.parse(s.substring(2), EmptyTerm())
          (res._1, correctAbstraction(atoms.reverse, res._2))
        }
        case ',' => parse(s.tail, atoms)
        case c => parse(s.tail, Atom(c) :: atoms)
      }
    }
}
object CombParser {
   def parse(s: String, term: Term) : (String, Term) =
    if (s.isEmpty) (s, term)
    else {
      s.charAt(0) match {
        case '(' => {
          val res = parse(s.substring(1), EmptyTerm())
          term match {
            case t : EmptyTerm => parse(res._1, res._2)
            case t => parse(res._1, Application(t, res._2))
          }
        }
        case ')' => (s.substring(1), term)
        case '[' => {
          val res = AbstractionParser.parse(s.substring(1), List.empty)
          term match {
            case t : EmptyTerm => parse(res._1, res._2)
            case t => parse(res._1, Application(t, res._2))
          }
        }
        case ' ' => parse(s.tail, term)
        case c => {
          term match {
            case t : EmptyTerm => parse(s.tail, Atom(c))
            case t => parse(s.tail, Application(t, Atom(c)))
          }
        }
      }
    }
}