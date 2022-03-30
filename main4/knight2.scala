// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if((x._1 >= 0 && x._1 < dim) && (x._2 >= 0 && x._2 < dim)) { // checks if the position is valid (not out of bounds or negative)
    if(path.contains(x)) {
      false
    }
    else {
      true
    }
  }
  else {
    false
  }
}

def knight_jumps() : List[Pos] = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2))

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val moves = for(jump <- knight_jumps()) yield (x._1 + jump._1, x._2 + jump._2)
  moves.filter(pos => is_legal(dim, path, pos))
}


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val possibleDestinations = legal_moves(dim, path, x)
    possibleDestinations.sortBy(destination => legal_moves(dim,path,destination).size)
}

//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 
import scala.annotation.tailrec

@tailrec
def first_closed_tour_heuristics_recursively(dim: Int, paths: List[Path]): Option[Path] = {
    if(paths.length == 0) {
        None
    }
    else {
        val path = paths.head
        val moves = for(jump <- knight_jumps()) yield (path.head._1 + jump._1, path.head._2 + jump._2)
        if(path.length == dim*dim && moves.contains(path.last)) {
            Some(path)
        }
        else {
            val nextMoves = ordered_moves(dim, path, path.head)
            val nextPaths = for(move <- nextMoves) yield move :: path
            first_closed_tour_heuristics_recursively(dim, nextPaths ::: paths.takeRight(paths.length - 1))
        }
    }
}


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = first_closed_tour_heuristics_recursively(dim, List(path))


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

@tailrec
def first_tour_heuristics_recursively(dim: Int, paths: List[Path]) : Option[Path] = {
    if(paths.length == 0) {
        None
    }
    else {
        val path = paths.head
        if(path.length == dim*dim) {
            Some(path)
        }
        else {
            val nextMoves = ordered_moves(dim, path, path.head)
            val nextPaths = for(move <- nextMoves) yield move :: path
            first_tour_heuristics_recursively(dim, nextPaths ::: paths.takeRight(paths.length - 1))
        }
    }
}

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = first_tour_heuristics_recursively(dim, List(path))

}
