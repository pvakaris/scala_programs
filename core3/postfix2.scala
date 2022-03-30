// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		          "-" -> 1,
		            "*" -> 2,
		            "/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_op(op: String) : Boolean = op match {
	case "+" | "-" | "*" | "/" | "^" => true
	case _ => false
}

def is_int(s : String) : Boolean = s.forall(_.isDigit)

def prec(op1: String, op2: String) : Boolean = precs(op1) >= precs(op2)

def addToOutput(stack: Toks, operation: String, toOutput: Toks = Nil) : Toks = {
  val operationAssociation = assoc(operation)
	if(stack != Nil && is_op(stack.head)) {
    operationAssociation match {
      case LA => {
        if(prec(stack.head, operation)) {
          addToOutput(stack.tail, operation, toOutput ::: List(stack.head)) 
        }
        else {
          toOutput
        }
      }
      case RA => {
        if((precs(stack.head) > precs(operation))) {
          addToOutput(stack.tail, operation, toOutput ::: List(stack.head)) 
        }
        else {
          toOutput
        }
      }
    }
	}
  else {
    toOutput
  }
}

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
	case Nil => out ::: st // Append remaining operations to the output list
	case head :: tail =>if (is_op(head)) {
							head match {
								case "+" => if (addToOutput (st, "+") != Nil) {  // If the precedence number of the case operator is smaller than the precedence number of the top operator of the stack (there are operators to add to the output List)
												val stack = List("+") ::: st.takeRight (st.length - addToOutput(st, "+").length)   // Then new stack is going to be = case operator + stack without those elements that have higher precedence number.
												val output = out ::: addToOutput (st, "+") // Those elements that have higher precedence number go to the output List immediately.

												syard(tail, stack, output) 
											}
											else {
												syard(tail, List("+") ::: st, out)
											}

								case "-" => if (addToOutput (st, "-") != Nil) {
												val stack = List("-") ::: st.takeRight (st.length - addToOutput(st, "-").length)
												val output = out ::: addToOutput (st, "-")

												syard(tail, stack, output) 
											}
											else {
												syard(tail, List("-") ::: st, out)
											}

								case "*" => if (addToOutput (st, "*") != Nil) {
												val stack = List("*") ::: st.takeRight (st.length - addToOutput(st, "*").length)
												val output = out ::: addToOutput (st, "*")

												syard(tail, stack, output) 
											}
											else {
												syard(tail, List("*") ::: st, out)
											}

								case "/" => if (addToOutput (st, "/") != Nil) {
												val stack = List("/") ::: st.takeRight(st.length - addToOutput(st, "/").length)
												val output = out ::: addToOutput (st, "/")

									 			syard(tail, stack, output) 
									 		}
											else {
												syard(tail, List("/") ::: st, out)
											}
                case "^" => if (addToOutput (st, "/") != Nil) {
												val stack = List("^") ::: st.takeRight(st.length - addToOutput(st, "^").length)
												val output = out ::: addToOutput (st, "^")

									 			syard(tail, stack, output) 
									 		}
											else {
												syard(tail, List("^") ::: st, out)
											}
							}
						}
						else if(is_int(head)) {
							if(toks.length == 1) {
								syard(Nil, Nil, out ::: List(head) ::: st)
							}
							else {
								syard(tail, st, out ::: List(head))
							}
						}
						else {
							head match {
								case "(" => {
									syard(tail, (List ("(") ::: st), out)
								}
								case ")" => {
									val output = out ::: st.slice(0, st.indexOf ("(") )
									val stack = st.takeRight(st.length - (st.indexOf ("(") + 1) )

									syard(tail, stack, output)
								}
							}
						}
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match{
	case Nil => st.head  // The result is going to be always at the top of the stack
	case head :: tail =>if(is_int(head)) {
							compute(tail, st ::: List(head.toInt))
						}
						else {
							head match {
								case "+" => {
									val stack = st.take(st.length - 2) ::: List(st(st.length - 2) + st(st.length - 1)) // New stack is penultimate element (case operation) the last element
									compute(tail, stack)
								}
								case "-" => {
									val stack = st.take(st.length - 2) ::: List(st(st.length - 2) - st(st.length - 1))
									compute(tail, stack)
								}
								case "*" => {
									val stack = st.take(st.length - 2) ::: List(st(st.length - 2) * st(st.length - 1))
									compute(tail, stack)
								}
								case "/" => {
									val stack = st.take(st.length - 2) ::: List(st(st.length - 2) / st(st.length - 1))
									compute(tail, stack)
								}
                case "^" => {
									val stack = st.take(st.length - 2) ::: List(BigInt(st(st.length - 2)).pow(st(st.length - 1)).toInt)
									compute(tail, stack)
								}
							}
						}
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
