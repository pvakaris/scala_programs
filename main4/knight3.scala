// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

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

import scala.annotation.tailrec

@tailrec
def tour_on_mega_board_recursively(dim: Int, paths: List[Path]) : Option[Path] = {
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
            tour_on_mega_board_recursively(dim, nextPaths ::: paths.takeRight(paths.length - 1))
        }
    }
}

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = tour_on_mega_board_recursively(dim, List(path))

}
