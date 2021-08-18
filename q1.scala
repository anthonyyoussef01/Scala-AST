package question1

/*
Question 1 (AST interpretation)

Write an evaluator (called eval) for the following AST, except for Let and Subs. You should fill in "eval" so it goes
from a Calc and turns it into an int.

During your pairing interview, you'll extend this to use the Let and Subs statements. For example: 
Let x equal 3 in y = 3 * x + 4
 */

import question1.Question1.AddReduce
import scalaz.Reducer

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

/*
Notes:
  - "var" is mutable, "val" is immutable
  - But you can do the following:
      val customers = collections.mutable.Map(100 -> "Mark", 101 -> "Halie")
      customers(100) = "Anthony"
  - No "break" or "continue", but "return" exists
  - Function:
      def funcName (param1:dataType, param2:dataType) : returnType = {
        function body
        return valueToReturn
      }
  - Tuples like python:
      var myTuple = (101, "Tony", 4.13)
  - Like Java, there's "protected", "private", and "public"
  - There's really no 'default constructor' in Scala like in Java,
    but it is just the first few lines of code in the class
  - We can create other constructors using "def this(...){ ... }"
  - There are not static methods and variables like in Java
  - You can use the keyword "override" to override methods like "toString()" as such:
      override def toString() : String = { ... }
  - Scala has abstract classes, but classes cannot extend multiple abstract classes. But,
    they can extend multiple traits
  - Like DrRacket, Scala has map, filter, fold, foldLeft, and foldRight
  - try, catch, and finally works the same way as in Java
*/

object Question1 {
  sealed trait Calc
  sealed trait Op extends Calc
  sealed trait Reducer
  case class Add(lhs: Calc, rhs: Calc) extends Op
  case class Mul(lhs: Calc, rhs: Calc) extends Op
  case class Lit(i: Long) extends Calc
  case object AddReduce extends Reducer
  case object MulReduce extends Reducer
  //let x = 7, x+5
  // symbol = expr, body
  case class Let(expr: Calc, sym: Symbol, body: Calc) extends Calc
  case class Subs(sym: Symbol) extends Calc
  case class Stmts(stmts: List[Calc], reduce_op: Reducer) extends Calc

  implicit def LitSyntax(i: Long): Lit = Lit(i)

  type Environment = String => Int
  /*
  You fill this in.
   */
  def eval(stmt: Calc): Int = {
    /*
    if (stmt.type == Add) {
      return
    }
    */
    val symbols = mutable.Map.empty[Symbol, Int]
    stmt match {
      case Add(lhs, rhs) =>
        return evalHelper(lhs, symbols) + evalHelper(rhs, symbols)
      case Mul(lhs, rhs) =>
        return evalHelper(lhs, symbols) * evalHelper(rhs, symbols)
      case Lit(i) =>
        return i.toInt
      case Stmts(stmts: List[Calc], reduce_op: Reducer) =>
        //for (statement <- stmts) {  }
        reduce_op match {
          case _: AddReduce.type => return stmts.foldRight(0)(evalHelper(_, symbols) + evalHelper(_, symbols))
          case _: MulReduce.type => return stmts.foldRight(0)(evalHelper(_, symbols) * evalHelper(_, symbols))
        }
      case Subs(sym) =>
        symbols(sym)

      //let x = 7, x+5
      // symbol = expr, body
      case Let(expr: Calc, sym: Symbol, body: Calc) =>
        symbols += (sym -> evalHelper(expr, symbols))
        return evalHelper(body, symbols)
    }
  }
  def evalHelper(stmt: Calc, symbols: mutable.Map[Symbol, Int]): Int = {
    /*
    if (stmt.type == Add) {
      return
    }
    */
    stmt match {
      case Add(lhs, rhs) =>
        return evalHelper(lhs, symbols) + evalHelper(rhs, symbols)
      case Mul(lhs, rhs) =>
        return evalHelper(lhs, symbols) * evalHelper(rhs, symbols)
      case Lit(i) =>
        return i.toInt
      case Stmts(stmts: List[Calc], reduce_op: Reducer) =>
        //for (statement <- stmts) {  }
        reduce_op match {
          case _: AddReduce.type => return stmts.foldRight(0)(evalHelper(_, symbols) + evalHelper(_, symbols))
          case _: MulReduce.type => return stmts.foldRight(0)(evalHelper(_, symbols) * evalHelper(_, symbols))
        }
      case Subs(sym) =>
        symbols(sym)

      //let x = 7, x+5
      // symbol = expr, body
      case Let(expr: Calc, sym: Symbol, body: Calc) =>
        symbols += (sym -> evalHelper(expr, symbols))
        return evalHelper(body, symbols)
    }
  }

  /*
  Example test data:
   */
  val test =
    Stmts(List(Add(1, 2), Mul(5, 7), Mul(-1, 3), Mul(5, -1)), AddReduce)

  val pairtest1 = Stmts(
    List(Add(1, 2),
         Mul(4, 10),
         Let(Mul(10, 20), 'bind, Mul(Lit(10), Subs('bind)))),
    AddReduce);
  val pairtest2 =
    Stmts((0 to 10).map(Let(_, 'bind, Add(Subs('bind), 3))).toList, MulReduce);
  // 3 35 -3 -5
  println(eval(test))
  //let x = 7, x+5
  // symbol = expr, body
  // 'bind = _ in
  //3, 40,  Add(Subs('bind), 3)
  //
  println(eval(pairtest1))
  //
  println(eval(pairtest2))

  // <- click play to run.
  def main(args: Array[String]): Unit = {
    eval(test)
  }
}
