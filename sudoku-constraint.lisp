;; Constraint Propagation - Performance Tracking

;; Define global variable for tracking rule applications
(defvar *rule-applications* 0)

;; Function to measure execution time
(defun measure-time (fn &rest args)
  "Measure execution time of a function."
  (let ((start-time (get-internal-real-time)))
    (apply fn args)
    (/ (- (get-internal-real-time) start-time)
       internal-time-units-per-second)))

;; Modify Constraint Propagation to track rule applications
(defun eliminate (grid)
  "Eliminates impossible values from each cell based on Sudoku constraints."
  (loop for row from 0 below +size+ do
        (loop for col from 0 below +size+ do
              (let ((cell (get-cell grid row col)))
                (when (not (cell-found cell))
                  (let ((non-possible-values (get-non-possible-values grid row col)))
                    (setf (cell-possible-values cell)
                          (set-difference (cell-possible-values cell) non-possible-values))
                    (incf *rule-applications*)))))))

;; Function to evaluate Constraint Propagation performance
(defun evaluate-constraint-propagation ()
  "Evaluate performance of the Constraint Propagation Sudoku solver."
  (setf *rule-applications* 0)
  (let* ((grid (initialize-sudoku *example-puzzle*))
         (time-taken (measure-time #'constraint-solve grid)))
    (format t "Constraint Propagation Method~%Time Taken: ~F seconds~%Rule Applications: ~D~%"
            time-taken *rule-applications*)))

;; Example usage:
(evaluate-constraint-propagation)
