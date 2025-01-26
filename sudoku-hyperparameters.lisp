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

(defconstant +size+ 9)

;; Define Sudoku grid
(defparameter *sudoku-grid* (make-array (list +size+ +size+)
                                         :initial-contents '(
  (5 0 1 0 0 0 6 0 4)
  (0 9 0 3 0 6 0 5 0)
  (0 0 0 0 9 0 0 0 0)
  (4 0 0 0 0 0 0 0 9)
  (0 0 0 1 0 9 0 0 0)
  (7 0 0 0 0 0 0 0 6)
  (0 0 0 0 2 0 0 0 0)
  (0 8 0 5 0 7 0 6 0)
  (1 0 3 0 0 0 7 0 2))))

;; Initialize domain of possible values
(defun initialize-grid ()
  (let ((grid (make-array (list +size+ +size+))))
    (loop for i below +size+ do
      (loop for j below +size+ do
        (setf (aref grid i j)
              (if (zerop (aref *sudoku-grid* i j))
                  (loop for n from 1 to +size+ collect n)
                  (list (aref *sudoku-grid* i j)))))))
    grid))

(defparameter *cell-grid* (initialize-grid))

(defun get-row-values (grid row)
  (remove-duplicates (remove-if-not #'numberp (mapcan #'identity (aref grid row)))))

(defun get-column-values (grid col)
  (remove-duplicates (remove-if-not #'numberp (loop for i below +size+ collect (aref grid i col)))))

(defun get-box-values (grid row col)
  (let* ((box-size (floor (sqrt +size+)))
         (r-start (* box-size (floor row box-size)))
         (c-start (* box-size (floor col box-size)))
         (box-values '()))
    (loop for i from r-start below (+ r-start box-size) do
      (loop for j from c-start below (+ c-start box-size) do
        (push (aref grid i j) box-values)))
    (remove-duplicates (remove-if-not #'numberp box-values))))

(defun eliminate-constraints (grid)
  (loop for i below +size+ do
    (loop for j below +size+ do
      (when (> (length (aref grid i j)) 1)
        (let ((eliminated (append
                            (get-row-values grid i)
                            (get-column-values grid j)
                            (get-box-values grid i j))))
          (setf (aref grid i j) (set-difference (aref grid i j) eliminated)))))))

(defun apply-single-value-rule (grid)
  (loop for i below +size+ do
    (loop for j below +size+ do
      (when (= (length (aref grid i j)) 1)
        (setf (aref grid i j) (list (car (aref grid i j))))))))

(defun sudoku-solved-p (grid)
  (loop for i below +size+ always
    (loop for j below +size+ always (numberp (aref grid i j)))))

(defun solve-sudoku ()
  (loop until (sudoku-solved-p *cell-grid*) do
    (eliminate-constraints *cell-grid*)
    (apply-single-value-rule *cell-grid*))
  (print-sudoku *cell-grid*))

(defun print-sudoku (grid)
  (loop for i below +size+ do
    (loop for j below +size+ do
      (format t "~a " (if (listp (aref grid i j)) (car (aref grid i j)) (aref grid i j))))
    (format t "~%")))
