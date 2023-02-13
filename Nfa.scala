import scala.collection.mutable

class Nfa[A](var alphabet: List[Char], var q: Set[A], var init: A, var finall: Set[A], var funct: mutable.HashMap[(A, String), Set[A]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = ??? // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    funct.get((state, c.toString)) match {
      case Some(next) => next
      case None => Set();
    }
  } // TODO implement next

  def epsilon_closure(s: A): Set[A] = {
    funct.get((s, "eps")) match {
      case Some(value) => value
      case None => Set()
    }
  }


  def aux_accepts(init2:A, str:String): Boolean = {
    if (str == "" && isFinal(init2)) return true
    for (s <- (epsilon_closure(init2))) {
      if (str == "" && isFinal(s) || aux_accepts(s, str)) return true
    }
    if(str == "")
      return false

    var booll :Boolean = false
    var allNext: Set[A] = next(init2, str.head)
    var allNext2: Set[A] = (epsilon_closure(init2))
    for(s <- allNext) {
      booll = aux_accepts(s, str.tail) || booll
    }
    for(s <- allNext2) {
      booll = aux_accepts(s, str) || booll
    }
    booll
  }
  def accepts(str: String): Boolean = {
    if (str == "" && isFinal(init)) return true
    var booll = false
    if (str == "") {
      for (s <- (epsilon_closure(init))) {
        if (isFinal(s) || aux_accepts(s, str)) return true;
      }
      false
    } else {
      for(s <- next(init, str.head)) {
        booll = aux_accepts(s, str.tail) || booll
      }
      for(s <- (epsilon_closure(init))) {
        booll = aux_accepts(s, str) || booll
      }
      booll
    }
  } // TODO implement accep
  def getStates : Set[A] = {
    q;
  } // TODO implement getStates

  def isFinal(state: A): Boolean = {
     (finall.contains(state) == true)

  }  // TODO implement isFinal


}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  def STAR(nfa: Nfa[Int], pos: Int): Nfa[Int] ={

    var init = nfa.init
    var finall = nfa.finall
    nfa.funct += ((pos, "eps")-> Set(init, pos+1))
    nfa.funct += ((finall.head, "eps")-> Set(pos+1, init))
    nfa.init = pos
    nfa.finall = Set(pos+1)
    nfa.q += (pos, pos+1)
    nfa
  }
  def PLUS(nfa: Nfa[Int]): Nfa[Int] ={
    nfa
  }
  def CONCAT(nfa1: Nfa[Int], nfa2: Nfa[Int]): Nfa[Int] ={
    var nfa : Nfa[Int] = new Nfa[Int](
      List[Char](), Set(), 0, Set(), mutable.HashMap(

      ));
    var s1: Int = nfa1.finall.head
    var s2 = nfa2.init
    for(s <- nfa1.funct)
      println(s)
    for(s <- nfa2.funct)
      println(s)
    nfa.init = nfa1.init
    nfa.finall = Set(nfa2.finall.head)
    nfa.alphabet = nfa1.alphabet ++ nfa2.alphabet
    nfa.q = nfa1.q ++ nfa2.q
    nfa.funct = nfa1.funct ++ nfa2.funct
    nfa.funct += ((s1, "eps") -> Set(s2))
    nfa
  }
  def UNION(nfa1: Nfa[Int], nfa2: Nfa[Int], pos: Int): Nfa[Int] ={
    var nfa : Nfa[Int] = new Nfa[Int](
      List[Char](), Set(), 0, Set(), mutable.HashMap(

      ));
    var init1 = nfa1.init
    var init2 = nfa2.init
    var final1 = nfa1.finall.head
    var final2 = nfa2.finall.head
    nfa.init = pos
    nfa.finall = Set(pos+1)
    nfa.alphabet = nfa1.alphabet ++ nfa2.alphabet
    nfa.q = nfa1.q ++ nfa2.q
    nfa.funct = nfa1.funct ++ nfa2.funct
    nfa.funct += ((pos, "eps")-> Set(init1, init2))
    nfa.funct += ((final1, "eps")-> Set(pos+1))
    nfa.funct += ((final2, "eps")-> Set(pos+1))
    nfa.q += (pos, pos+1)
    nfa
  }
  def fromPrenex(str: String): Nfa[Int] = { // TODO implement Prenex -> Nfa transformation.
    var stack = mutable.Stack[(String, Int)]();
    var splitt = (str.split(" ")).toBuffer
    var i = 0;

    for(i <- 1 to splitt.length-1) {
      if(splitt(i-1).equals("'") && splitt(i).equals("'")){
        splitt(i-1) = " "
        splitt -= splitt(i)
      }
    }
    for(i <- 0 to splitt.length-1) {
      if(splitt(i)(0) == '\''){
        splitt(i) = splitt(i)(splitt(i).length -2).toString

      }
    }
     println(splitt.toList);
    for (elem <- splitt){
      if(elem.length == 1)
        stack.push((elem, 0));
      else if(elem == "CONCAT")
        stack.push((elem, 2))
      else if(elem == "PLUS")
        stack.push((elem, 1))
      else if(elem == "UNION")
        stack.push((elem, 2))
      else if(elem == "STAR")
        stack.push((elem, 1))
      else stack.push((elem, 0))

    }
    println(stack.toList)
    var stack_nfa = mutable.Stack[Nfa[Int]]();
    var pos = 0;
    while(!stack.isEmpty){
      var tuple = stack.top
      if(tuple._2 == 0) {
        var nfa2: Nfa[Int] = null
        if(tuple._1 != "void" && tuple._1 != "eps") {
          nfa2 = new Nfa[Int](
            List[Char](tuple._1(0)), Set(pos, pos + 1), pos, Set(pos + 1), mutable.HashMap(
              (pos, tuple._1) -> Set(pos + 1)
            ));
          pos += 2
          stack_nfa.push(nfa2);
        }
        else if(tuple._1 == "void"){
          nfa2 = new Nfa[Int](
            List[Char](), Set(pos, pos + 1), pos, Set(pos + 1), mutable.HashMap(

            ));
          pos += 2
          stack_nfa.push(nfa2);
        }
        else{
          nfa2 = new Nfa[Int](
            List[Char](), Set(pos, pos+1), pos, Set(pos), mutable.HashMap(
              (pos, tuple._1) -> Set(pos+1)
            ));
          pos += 1
          stack_nfa.push(nfa2);
        }
        stack.pop();

      }
      else if(tuple._1 == "STAR"){
        stack.pop();
        var nfa2 = stack_nfa.top
        var new_nfa = STAR(nfa2, pos)
        stack_nfa.pop()
        stack_nfa.push(new_nfa)
        pos += 2
      }
      else if(tuple._1 == "PLUS"){
        stack.pop();
        var nfa2 = stack_nfa.top
        var new_nfa = PLUS(nfa2)
        stack_nfa.pop()
        stack_nfa.push(new_nfa)
      }
      else if(tuple._1 == "CONCAT"){
        stack.pop();
        var nfa2 = stack_nfa.top
        stack_nfa.pop()
        var nfa3 = stack_nfa.top
        stack_nfa.pop()
        var new_nfa = CONCAT(nfa2, nfa3)
        stack_nfa.push(new_nfa)
      }
      else if(tuple._1 == "UNION"){
        stack.pop();
        var nfa2 = stack_nfa.top
        stack_nfa.pop()
        var nfa3 = stack_nfa.top
        stack_nfa.pop()
        var new_nfa = UNION(nfa2, nfa3, pos)
        stack_nfa.push(new_nfa)
        pos += 2
      }
    }
    var nfa = stack_nfa.top
    nfa
  }
  
}

object Main {
  def main(args: Array[String]): Unit = {
    var nfa1: Nfa[Int] = new Nfa[Int](
      List[Char](), Set(), 0, Set(2), mutable.HashMap(
        ((0, "c") -> Set(0)), ((1, "") -> Set(2)), ((1, "c") -> Set(2))
      )
    )
    var nfa2: Nfa[Int] = new Nfa[Int](
      List[Char]('a', 'b'), Set(2,3,0,1), 2, Set(1), mutable.HashMap(
        ((2, "a") -> Set(3)), ((3, "eps") -> Set(0)), ((0, "b") -> Set(1))
      ))

  }
}