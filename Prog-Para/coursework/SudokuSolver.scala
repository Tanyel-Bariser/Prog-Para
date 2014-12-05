import scala.annotation.tailrec
import java.io.File
import javax.swing.JFileChooser
import javax.swing.JOptionPane

/** Partial Sudoku solver solves the logical answers
  * but doesn't use guessing and backtracking.
  * The set of possible digits are printed out for the
  * remaining unsolved positions.
  *
  * @author Tanyel Bariser
  * @course MSc Computer Science
  * @deadline Sunday, 12th January, 23:55
  */
object SudokuSolver {
  /** Main method allows user to choose file and it controls Sudoku Solver */
	def main(args: Array[String]) = {
		val chooser = new JFileChooser()
        val checker = chooser.showOpenDialog(null)
		val namePath = chooser.getSelectedFile()
		if (checker == JFileChooser.APPROVE_OPTION) {
			val puzzle = readSudokuTxt(namePath.getAbsolutePath())
			println("\n\nThe following is the full/partial answer to the puzzle:")
			prettyPrint(solve(puzzle))
		} else {
			JOptionPane.showMessageDialog(null, "You have clicked Cancel.")
		}
	}
	
  /** Reads and prints Sudoku text file
	*
	* @param title of text file to read containing Sudoku puzzle
	* @return Sudoku puzzle as an array of lists
	*/
	def readSudokuTxt(title: String): Array[List[Char]] = {
		val source = io.Source.fromFile(title)
		val puzzleLines = source.getLines.toArray
		source.close
		val puzzleNoBlankLines = puzzleLines.filter(_.size > 0)
		val puzzleNoSpaces = puzzleNoBlankLines.map(_.replaceAll(" ", "").replaceAll("\t", ""))
		val puzzle = puzzleNoSpaces.map(_.toList)
		println("The following is the puzzle that has been read in from file " + title)
		prettyPrint(puzzle)
		puzzle
	}
	
  /** Prints Sudoku puzzle with pretty borders
	*
	* @param puzzle to be printed
	*/
	def prettyPrint(puzzle: Array[List[Char]]) = {
		val prettyPuzzle = for {i <- 0 to 8
								j <- 0 to 8}
									yield puzzle(i)(j)
		for(i <- 1 to prettyPuzzle.size) {
			if (i == 1 || i == 28 || i == 55) print("\n+---+---+---+\n|")
			print(prettyPuzzle(i-1))
			if (i % 3 == 0)	print("|")
			if (i % 9 == 0 && !(i == 27 || i == 54 || i == 81)) print("\n|")
			if (i == 81) print("\n+---+---+---+\n")
		}
	}	

  /** Tail recursion optimised method responsible for controlling
	* the logic toward solving the Sudoku puzzle
	* 
	* @param puzzle to be solved
	* @return fully/partially solved Sudoku puzzle
	*/
	@tailrec def solve(puzzle: Array[List[Char]]): Array[List[Char]] = {
		val unknownPositions = for {i <- 0 to 8
									j <- 0 to 8
									if (puzzle(i)(j) == '_')}
										yield (i, j)
		val knownPositions = for{position <- unknownPositions
								digit <- knownDigit(position, puzzle)}
									yield(position, digit)
		val solved = knownPositions.isEmpty
		if(solved) {
			if (!unknownPositions.isEmpty) {
				println("\nFirst lists the set of possible digits for each unknown position:\n")
				unknownPositions.foreach{case(rowNum, columnNum) =>
						println("Row " + (rowNum+1) + ", Column " + (columnNum+1) + ": is " + possibleDigits((rowNum,columnNum), puzzle))}
			}
			puzzle
		} else {
			val updatedRows = knownPositions.map{case(position, digit) => puzzle(position._1)updated(position._2, digit)}
			val updatedPuzzle = updatePuzzle(knownPositions, updatedRows, puzzle)
			solve(updatedPuzzle)
		}
	}

  /** Only returns a digit if it's the single correct answer for the row/column position
	*
	* @param position (row/column) of puzzle
	* @param puzzle to be solved
	* @return either the correct digit to a previously unknown position or none 
	*/
	def knownDigit(position: (Int, Int), puzzle: Array[List[Char]]): Option[Char] = {
		val knownDigit = possibleDigits(position, puzzle)
		if(knownDigit.size == 1) knownDigit.headOption
		else None
	}
	
  /** Takes an unknown position of a Sudoku puzzle and returns a set of digits that
	* could potentially fill that position.
	* 
	* Finds the corresponding row, column and 3 by 3 box digits and removes those as
	* potential digits that could correctly fill that position.
	*
	* @param position (row/column) of puzzle
	* @param puzzle to be solved
	* @return the set of possible digits that could fill the position
	*/
	def possibleDigits(position: (Int, Int), puzzle: Array[List[Char]]): Set[Char] = {
		import math.ceil
		val rowNum = position._1
		val columnNum = position._2
		val rowDigits = for{i <- 0 to 8
							if (puzzle(rowNum)(i) != '_')}
								yield puzzle(rowNum)(i)
		val columnDigits = for {i <- 0 to 8
								if (puzzle(i)(columnNum) != '_')}
									yield puzzle(i)(columnNum)		
		val (xBox, yBox) = (ceil((rowNum + 1) / 3.0).toInt, ceil((columnNum + 1) / 3.0).toInt)
		val (xBoxTop, yBoxTop) = (xBox * 3, yBox * 3)
		val (xBoxBottom, yBoxBottom) = (xBoxTop - 2, yBoxTop - 2)
		val boxDigits = for {
			x <- xBoxBottom to xBoxTop
			y <- yBoxBottom to yBoxTop
		} yield puzzle(x-1)(y-1)

		(1 to 9).mkString.toSet -- (rowDigits.toSet ++ columnDigits.toSet ++ boxDigits.toSet)
	}

  /** Tail recursion optimised method updates puzzle and returns it.
	* Since puzzle is an array of lists, this method replaces one list in the puzzle array
	* with a list contained in updatedRows at each recursive iteration.
	*
	* @param knownPositions contains row numbers of most recently calculated positions
	* @param updatedRows lists of rows most recently updated
	* @param puzzle to be solved
	* @return puzzle updated with most recent calculated answers
	*/
	@tailrec def updatePuzzle(knownPositions: IndexedSeq[((Int, Int), Char)],
			updatedRows: IndexedSeq[List[Char]], puzzle: Array[List[Char]]): Array[List[Char]] = {

		if(updatedRows.isEmpty) {
			puzzle
		} else {
			val rowNum = knownPositions(0)._1._1
			val updatedPuzzle = puzzle.updated(rowNum, updatedRows(0))
			updatePuzzle(knownPositions.tail, updatedRows.tail, updatedPuzzle)
		}
	}
}