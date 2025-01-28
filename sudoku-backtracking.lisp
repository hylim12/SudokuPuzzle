;; Backtracking Algorithm - Performance Tracking

;; Define global variable for tracking recursive calls
(defvar *recursive-calls* 0)

;; Function to measure execution time
(defun measure-time (fn &rest args)
  "Measure execution time of a function."
  (let ((start-time (get-internal-real-time)))
    (apply fn args)
    (/ (- (get-internal-real-time) start-time)
       internal-time-units-per-second)))

;; Modify Backtracking to track recursive calls
(defun solve-sudoku (grid &optional (row 0) (col 0))
  "Solve the Sudoku puzzle using backtracking."
  (incf *recursive-calls*)
  (if (not (find-empty-location grid row col))
      t  ;; No empty location means puzzle is solved
      (loop for num from 1 to +n+
            when (is-safe grid row col num)
            do (setf (aref grid row col) num)
               (when (solve-sudoku grid row col)
                 (return t))
               (setf (aref grid row col) 0))  ;; Undo move if unsuccessful
      nil))

;; Function to evaluate Backtracking performance
(defun evaluate-backtracking ()
  "Evaluate performance of the Backtracking Sudoku solver."
  (setf *recursive-calls* 0)
  (let* ((grid (initialize-sudoku *example-puzzle*))
         (time-taken (measure-time #'solve-sudoku grid)))
    (format t "Backtracking Method~%Time Taken: ~F seconds~%Recursive Calls: ~D~%"
            time-taken *recursive-calls*)))

;; Usage: 
(evaluate-backtracking)

