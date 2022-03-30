// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million

def calculateCollatz(n: Long, steps: Long) : (Long, Long) = {
    if(n == 1) (n, steps)
    else if (n % 2 == 0) calculateCollatz((n/2), steps+1)
    else calculateCollatz((3*n+1), steps+1)
}

def collatz(n: Long) : Long = {
    calculateCollatz(n, 0L)._2 
}

//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.
  
def collatz_max(bnd: Long) : (Long, Long) = {
    val list = (1L to bnd).toList          
    val collatzList = list.map(collatz)
    val biggest = collatzList.max
    val indexOfBiggestCollatz = collatzList.indexOf(biggest)
    (biggest, list(indexOfBiggestCollatz))
}

//(3) Implement a function that calculates the last_odd
//    number in a collatz series.  For this implement an
//    is_pow_of_two function which tests whether a number 
//    is a power of two. The function is_hard calculates 
//    whether 3n + 1 is a power of two. Again you can
//    assume the input ranges between 1 and 1 Million,
//    and also assume that the input of last_odd will not 
//    be a power of 2.

def is_pow_of_two(n: Long) : Boolean = {
    val temp = n - 1
    (n != 0) && ((n & temp) == 0)
}

def is_hard(n: Long) : Boolean = is_pow_of_two((3*n) + 1)

def last_odd_collatz(n: Long, answer: List[(Long, Boolean)]) : List[(Long, Boolean)] = {
    if(n == 1) {
        answer
    } 
    else if(n % 2 == 0) {
        if(is_pow_of_two(n)) {
            answer match {
                case Nil => last_odd_collatz((n/2), Nil)
                case head :: tail => {
                    if(tail != Nil) {
                        last_odd_collatz((n/2), List((tail.head._1, true)))
                    }
                    else {
                        last_odd_collatz((n/2), List((head._1, true)))
                    }
                }
            }
        }
        else {
            last_odd_collatz((n/2), answer)
        }
    } 
    else {
        answer match {
            case Nil => last_odd_collatz((3*n+1), List((n, false)))
            case head :: tail => {
                if(tail != Nil) {
                    last_odd_collatz((3*n+1), List(head, (n, false)))
                }
                else {
                    if(head._2 == true) {
                        last_odd_collatz((3*n+1), List(head, (n, false)))
                    }
                    else {
                        last_odd_collatz((3*n+1), List((n, false)))
                    }
                }
            }
        }
    }
}

def last_odd(n: Long) : Long = {
    if(n % 2 == 0) {
        val answer = last_odd_collatz(n, Nil)
        answer match {
            case Nil => 0
            case head :: tail => {
                if(head._2 == true) {
                    head._1
                }
                else {
                    0
                }
            }
        }
    }
    else {
        val answer = last_odd_collatz(n, List((n, false)))
        answer match {
            case Nil => 0
            case head :: tail => {
                if(head._2 == true) {
                    head._1
                }
                else {
                    0
                }
            }
        }
    }
}

}



