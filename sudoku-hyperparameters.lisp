(defpackage :sudoku-backtracking
  (:use :cl))

(in-package :sudoku-backtracking)

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

;; Finds an empty cell (represented as 0)
(defun find-empty-cell (board)
  (loop for row from 0 below 9
        do (loop for col from 0 below 9
                 when (= (aref board row col) 0)
                 do (return-from find-empty-cell (values row col))))
  nil)

;; Checks if placing a number in a specific cell is valid
(defun is-valid (board row col num)
  (or (not (member num (coerce (row-values board row) 'list)))
      (not (member num (coerce (col-values board col) 'list)))
      (not (member num (coerce (box-values board row col) 'list)))))

;; Solves Sudoku using backtracking
(defun backtracking-solve (board)
  (multiple-value-bind (row col) (find-empty-cell board)
    (if (not row)
        t
        (loop for num from 1 to 9
              do (when (is-valid board row col num)
                   (setf (aref board row col) num)
                   (when (backtracking-solve board) (return t))
                   (setf (aref board row col) 0)))
        nil)))

;; Wrapper function to solve Sudoku using backtracking
(defun solve-sudoku-backtracking (puzzle)
  (let ((board (parse-sudoku puzzle)))
    (backtracking-solve board)
    board))

;; Solves all Sudoku puzzles in the dataset using backtracking
(defun solve-sudoku-dataset-backtracking (filename)
  (mapcar #'(lambda (row) (solve-sudoku-backtracking (first row))) (read-sudoku-dataset filename)))


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
