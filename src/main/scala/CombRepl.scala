/**
 * Created with IntelliJ IDEA.
 * User: Sergey Fironov
 * Date: 22.07.13
 * Time: 11:42
 */

import scala.util.control._

object CombRepl extends App {
  val break = new Breaks
  break.breakable{
    do {
      Console.print("ξ>")
      val s = Console.readLine()
      if (s != "q:"){
        val res = CombParser.parse(s, EmptyTerm())
        Console.println("Parse: " + res._1 + "; " + res._2)
        val resolveAbstraction = AbstractTransformer(res._2)
        Console.println("Resolve abstraction: " + TermPrinter(resolveAbstraction))
        val weakReduction = WeakReductor(resolveAbstraction)
        Console.println(TermPrinter(weakReduction))
      }else break.break()
    } while (true)
  }
}
