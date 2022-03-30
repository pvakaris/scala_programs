// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
				"-" -> 1,
				"*" -> 2,
				"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = op match {
	case "+" | "-" | "*" | "/" => true
	case _ => false
}

def is_int(s : String) : Boolean = s.forall(_.isDigit)

def prec(op1: String, op2: String) : Boolean = precs(op1) >= precs(op2)

def addToOutput(stack: Toks, operation: String, toOutput: Toks = Nil) : Toks = {
	if( stack != Nil && is_op(stack.head) && prec(stack.head, operation) ) {
		addToOutput(stack.tail, operation, toOutput ::: List(stack.head)) 
	}
	else {
		toOutput
	}
}


// Not working properly. Using a shortened version instead
def addToOutputParenthesis(stack: Toks, toOutput: Toks = Nil) : Toks = stack match{
	case Nil => toOutput
	case head :: tail =>if(head == "(") {
							toOutput
						}
						else {
							addToOutputParenthesis(tail, toOutput ::: List(head))
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
									val output = out ::: st.slice(0, st.indexOf ("(") ) // slice() function found on stackoverflow
									val stack = st.takeRight(st.length - (st.indexOf ("(") + 1) )

									syard(tail, stack, output)
								}
							}
						}
}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as arguments. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

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
							}
						}

}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}
