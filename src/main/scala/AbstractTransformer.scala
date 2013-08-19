/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 16.08.13
 * Time: 17:03
 */
object AbstractTransformer {
  def apply(t: Term) : Term =
    t match {
      case Abstraction(Atom(c1), Atom(c2)) => {
        if (c1 == c2) Atom('I')
        else Application(Atom('K'), Atom(c2))
      }
      case Abstraction(Atom(c1), Application(u, Atom(x))) => {
        val atoms = MetaTerm.atoms(u)
        if (! atoms.contains(Atom(c1)) && x == c1) AbstractTransformer(u)
        else{
          Application(
            Application(
              Atom('S'),
              AbstractTransformer(Abstraction(Atom(c1), AbstractTransformer(u)))),
            AbstractTransformer(Abstraction(Atom(c1), Atom(x)))
          )
        }
      }
      case Abstraction(Atom(c1), Application(u, v)) => {
        val atomsU = MetaTerm.atoms(u)
        val atomsV = MetaTerm.atoms(v)
        if (! atomsU.contains(Atom(c1)) && ! atomsV.contains(Atom(c1))){
          Application(Atom('K'), Application(AbstractTransformer(u), AbstractTransformer(v)))
        }
        else{
          Application(
            Application(
              Atom('S'),
              AbstractTransformer(Abstraction(Atom(c1), AbstractTransformer(u)))),
            AbstractTransformer(Abstraction(Atom(c1), AbstractTransformer(v)))
          )
        }
      }
      case Abstraction(Atom(c1), Abstraction(a, t1)) => {
        AbstractTransformer(
          Abstraction(Atom(c1), AbstractTransformer(Abstraction(a,t1)))
        )
      }
      case Application(t1, t2) => Application(AbstractTransformer(t1), AbstractTransformer(t2))
      case _ => t
    }
}
