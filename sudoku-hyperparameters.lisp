;; Backtracking Algorithm
;; Define a constant for grid size (9X9 Sudoku grid)
(defconstant +n+ 9)

;; Function to check if a number can be placed in a given cell
(defun is-safe (grid row col num)
  "Check if it's safe to place num in grid[row][col]"
  (loop for x from 0 below +n+
        when (or (= (aref grid row x) num)
                 (= (aref grid x col) num)
                 (= (aref grid (+ (* 3 (/ row 3)) (mod x 3))
                                  (+ (* 3 (/ col 3)) (floor x 3))) num))
        do (return nil))
  t)

;; Function to find an empty location
(defun find-empty-location (grid row col)
  "Find an empty location in the grid"
  (loop for r from row below +n+
        do (loop for c from (if (= r row) col 0) below +n+
                 when (= (aref grid r c) 0)
                 do (setf row r col c) (return t)))
  nil)

;; Function to solve Sudoku using Backtracking
(defun solve-sudoku (grid &optional (row 0) (col 0))
  "Solve the Sudoku puzzle using backtracking"
  (if (not (find-empty-location grid row col))
      t  ; No empty location means puzzle is solved
      (loop for num from 1 to +n+
            when (is-safe grid row col num)
            do (setf (aref grid row col) num)
               (when (solve-sudoku grid row col)
                 (return t))
               (setf (aref grid row col) 0))  ; Undo move if unsuccessful
      nil))

;; Function to print the grid
(defun print-grid (grid)
  "Print the Sudoku grid"
  (loop for i below +n+
        do (loop for j below +n+
                 do (format t "~d " (aref grid i j)))
           (format t "~%")))


;; ------------------ Constraint Propagation Implementation ------------------

(defpackage :sudoku-constraint-propagation
  (:use :cl))

(in-package :sudoku-constraint-propagation)

;; Reads a CSV file containing Sudoku puzzles
(defun read-sudoku-dataset (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect (split-sequence #\, line))))

;; Converts a flat string Sudoku representation into a 2D array
(defun parse-sudoku (puzzle)
  (let ((board (make-array '(9 9) :initial-element 0)))
    (loop for i from 0 below 81
          for row = (floor i 9)
          for col = (mod i 9)
          do (setf (aref board row col) (digit-char-p (char puzzle i))))
    board))

;; Applies constraint propagation to reduce possible values
(defun apply-constraint-propagation (board)
  (loop for row from 0 below 9
        do (loop for col from 0 below 9
                 when (= (aref board row col) 0)
                 do (let ((possible-values (find-possible-values board row col)))
                      (when (= (length possible-values) 1)
                        (setf (aref board row col) (car possible-values)))))))
  board)

;; Wrapper function to solve Sudoku using constraint propagation
(defun solve-sudoku-constraint-propagation (puzzle)
  (let ((board (parse-sudoku puzzle)))
    (apply-constraint-propagation board)
    board))

;; Solves all Sudoku puzzles in the dataset using constraint propagation
(defun solve-sudoku-dataset-constraint-propagation (filename)
  (mapcar #'(lambda (row) (solve-sudoku-constraint-propagation (first row))) (read-sudoku-dataset filename)))

;;; COPY AND RUN THE COMMANDS BELOW IN TERMINAL

;; Load and use the Backtracking Algorithm
(ql:quickload :sudoku-backtracking)
;; Example usage with a sample Sudoku puzzle
(let ((sample-puzzle "530070000600195000098000060800060003400803001700020006060000280000419005000080079"))
  (format t "Solving Sudoku with Backtracking:~%")
  (solve-sudoku-backtracking sample-puzzle))
;; Load and use the Constraint Propagation Algorithm
(ql:quickload :sudoku-constraint-propagation)
;; Example usage with the same sample puzzle
(let ((sample-puzzle "530070000600195000098000060800060003400803001700020006060000280000419005000080079"))
  (format t "Solving Sudoku with Constraint Propagation:~%")
  (solve-sudoku-constraint-propagation sample-puzzle))
;; Solve all puzzles from dataset using Backtracking
(let ((dataset-file "sudoku.csv"))
  (format t "Solving all Sudoku puzzles using Backtracking:~%")
  (solve-sudoku-dataset-backtracking dataset-file))
;; Solve all puzzles from dataset using Constraint Propagation
(let ((dataset-file "sudoku.csv"))
  (format t "Solving all Sudoku puzzles using Constraint Propagation:~%")
  (solve-sudoku-dataset-constraint-propagation dataset-file))
