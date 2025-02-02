;; Just for my own reference and understanding
;; Function to load Sudoku puzzles and solutions from CSV
(defun load-sudoku-dataset (filename)
  "Reads a Sudoku dataset from a CSV file and stores puzzles and solutions in a hash table."
  (let ((solutions (make-hash-table)))  ;; Create a hash table
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil) 
            while line do
        (let* ((data (split-string line ","))  ;; Split line by comma
               (puzzle (parse-sudoku (nth 0 data)))  ;; Convert puzzle to LISP list
               (solution (parse-sudoku (nth 1 data))))  ;; Convert solution to LISP list
          (setf (gethash puzzle solutions) solution))))  ;; Store in hash table
    solutions))  ;; Return hash table

;; Load dataset (automatically fills the hash table)
(defparameter *sudoku-solutions* (load-sudoku-dataset "sudoku.csv"))

;; Call evaluate-accuracy function to test the accuracy of each search method
(format t "Evaluating Accuracy...
(format t "DFS Accuracy: ~A%~%" (evaluate-accuracy #'dfs-sudoku-solver *sudoku-puzzles* *sudoku-solutions*))
(format t "A* Accuracy: ~A%~%" (evaluate-accuracy #'a-star-sudoku-solver *sudoku-puzzles* *sudoku-solutions*))

;; Call the measure-time function to measure the processing time for DFS
(format t "Measuring DFS Execution Time...~%")
(let ((time (measure-time #'dfs-sudoku-solver puzzle)))
  (format t "DFS took ~A milliseconds.~%" time))

;; Check the memory used in processing data using DFS
(check-memory-usage)

;; Call the measure-time function to measure the processing time for A* Search
(format t "Measuring A* Execution Time...~%")
(let ((time (measure-time #'a-star-sudoku-solver puzzle)))
  (format t "A* took ~A milliseconds.~%" time))

;; Check the memory used in processing data using A* Search
(check-memory-usage)

;; Call the evaluate-precision function to test the precision for each search method
(format t "Evaluating Precision...~%")
(format t "DFS Precision: ~A%~%" (evaluate-precision #'dfs-sudoku-solver *sudoku-puzzles* *sudoku-solutions*))
(format t "A* Precision: ~A%~%" (evaluate-precision #'a-star-sudoku-solver *sudoku-puzzles* *sudoku-solutions*))

