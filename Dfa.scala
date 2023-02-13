import scala.collection.mutable
import scala.reflect.ClassManifestFactory.Nothing
import scala.runtime.Nothing$
import scala.util.control.Breaks.break

class Dfa[A] (var alphabet: List[Char], var q: Set[A], var init: A, var finall: Set[A], var funct: mutable.HashMap[(A, Char), A], var sink: A){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
  def map[B](f: A => B) : Dfa[B] = ??? // TODO implement map


  def next(state:A, c: Char): A = {
    funct.get((state, c)) match {
      case Some(next) => next
      case None => sink
    }
  } // TODO implement next
  def aux_accepts(init2:A, str:String): Boolean = {
    if (str == "" && isFinal(init2)) return true

    if(str == "")
      return false

    var booll :Boolean = false
    var Next: A = next(init2, str.head)
    booll = aux_accepts(Next, str.tail) || booll
    booll
  }
  def accepts(str: String): Boolean = {
    if (str == "" && isFinal(init)) return true
    var booll = false
    if (str == "")
      false
    else {
      booll = aux_accepts(next(init, str.head), str.tail) || booll
      booll
    }
  } // TODO implement accepts

  def getStates : Set[A] = {
    q
  } // TODO implement getStates

  def isFinal(state: A): Boolean = {
    (finall.contains(state) == true)
  }  // TODO implement isFinal


}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def eps_cl(set: Set[Int], nfa: Nfa[Int]): Set[Int] = {
      var y : Set[Int] = Set()
      y = y ++ set
    println(y)
      for(cl <- set){
        var set2 = nfa.epsilon_closure(cl)
        y = y ++ (set ++ eps_cl(set2, nfa))
      }
    y
  }
  def fromPrenex(str: String): Dfa[Int] = { // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa
    var nfa = Nfa.fromPrenex(str)
    var closures = new mutable.HashMap[Int, Set[Int]]()
    var dfa = new Dfa[Int]((nfa.alphabet), Set(0, -1), 0, Set(), mutable.HashMap(

    ), -1)

    for(state <- nfa.q){
      var set = nfa.epsilon_closure(state)
      closures += (state -> (eps_cl(set, nfa) ++ Set(state)))
    }

    var l  : Set[Set[Int]] = Set()
    var lis  = mutable.Stack[Set[Int]]()
    var pos = 0
    var start = closures(nfa.init)
    for(c <- start)
      println(c)

    l += start
    lis.push(start)
    for(el <- start) {
      if(el == nfa.finall.head){
        dfa.finall += pos
      }
    }

    var pos_init = 0

    var map  =  mutable.HashMap(start -> 0)
    while(!lis.isEmpty){

      var states = lis.top//.head
      lis.pop
      pos_init = map.getOrElse(states, -1)
      for(c <- dfa.alphabet) {
        var next2: Set[Int] = Set()
        var next: Set[Int] = Set()
        for (state <- states) {
          for (i <- nfa.next(state, c))
            next = next ++ closures(i)
        }
        if (next.size > 0) {
          if (!l.contains(next)) {
            pos += 1
            map += (next -> pos)
            l += next
            lis.push(next)
            dfa.q += pos
            dfa.funct += ((pos_init, c) -> pos)
            for (el <- next) {
              if (el == nfa.finall.head) {
                dfa.finall += pos
              }
            }
          }
          else {
            var found_state: Int = map.getOrElse(next, -1)
            dfa.funct += ((pos_init, c) -> found_state)
            for (el <- next) {
              if (el == nfa.finall.head) {
                dfa.finall += pos
              }
            }
          }
        }
      }
    }

    dfa
  }

}
