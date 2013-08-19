/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 16.08.13
 * Time: 12:37
 */
object WeakReductor {
  def apply(t: Term): Term =
  t match {
    case Atom(c) => Atom(c)
    case Application(Atom('I'), t1) => {
      Console.println("Term: " + t)
      Console.println("Weak reduction on I: " + TermPrinter(t1))
      WeakReductor(t1)
    }
    case Application(Application(Atom('K'), t1), t2) => {
      Console.println("Term: " + t)
      Console.println("Weak reduction on K: " + TermPrinter(t1) + "," + TermPrinter(t2))
      WeakReductor(t1)
    }
    case Application(Application(Application(Atom('S'), t1), t2), t3) => {
      Console.println("Term: " + t)
      Console.println("Weak reduction on S: " + TermPrinter(t1) + "," + TermPrinter(t2) + "," + TermPrinter(t3))
      WeakReductor(Application(WeakReductor(Application(WeakReductor(t1),t3)), WeakReductor(Application(WeakReductor(t2), t3))))
    }
    case Application(t1, t2) =>
    {
      Console.println("Term: " + t)
      Console.println("Weak reduction on application: " + TermPrinter(t1) + "," + TermPrinter(t2))
      if (t1 == WeakReductor(t1) && t2 == WeakReductor(t2))
        Application(t1,t2)
      else
        WeakReductor(Application(WeakReductor(t1), t2))
    }
    case _ => t
  }
}
