(defpackage :sudoku-solver
  (:use :cl))

(in-package :sudoku-solver)

(defun read-sudoku-dataset (filename)
  "Reads a CSV file containing Sudoku puzzles and their solutions."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect (split-sequence #\, line))))

(defun parse-sudoku (puzzle)
  "Converts a flat string Sudoku representation into a 2D array."
  (let ((board (make-array '(9 9) :initial-element 0)))
    (loop for i from 0 below 81
          for row = (floor i 9)
          for col = (mod i 9)
          do (setf (aref board row col) (digit-char-p (char puzzle i))))
    board))

(defun find-empty-cell (board)
  "Finds an empty cell in the Sudoku board (represented as 0)."
  (loop for row from 0 below 9
        do (loop for col from 0 below 9
                 when (= (aref board row col) 0)
                 do (return-from find-empty-cell (values row col))))
  nil)

(defun is-valid (board row col num)
  "Checks whether placing NUM in (row, col) is valid."
  (or (not (member num (coerce (row-values board row) 'list)))
      (not (member num (coerce (col-values board col) 'list)))
      (not (member num (coerce (box-values board row col) 'list)))))

(defun backtracking-solve (board)
  "Solves the Sudoku puzzle using backtracking."
  (multiple-value-bind (row col) (find-empty-cell board)
    (if (not row)
        t
        (loop for num from 1 to 9
              do (when (is-valid board row col num)
                   (setf (aref board row col) num)
                   (when (backtracking-solve board) (return t))
                   (setf (aref board row col) 0)))
        nil)))

(defun apply-constraint-propagation (board)
  "Reduces possibilities by applying constraint rules before backtracking."
  (loop for row from 0 below 9
        do (loop for col from 0 below 9
                 when (= (aref board row col) 0)
                 do (let ((possible-values (find-possible-values board row col)))
                      (when (= (length possible-values) 1)
                        (setf (aref board row col) (car possible-values)))))))
  board)

(defun solve-sudoku (puzzle)
  "Solves a Sudoku puzzle by applying constraint propagation and backtracking."
  (let ((board (parse-sudoku puzzle)))
    (apply-constraint-propagation board)
    (backtracking-solve board)
    board))

(defun solve-sudoku-dataset (filename)
  "Solves all Sudoku puzzles in the dataset."
  (mapcar #'(lambda (row) (solve-sudoku (first row))) (read-sudoku-dataset filename)))
