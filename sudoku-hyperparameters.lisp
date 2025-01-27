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


;; Constraint Propagation Implementation
(defconstant +size+ 9)  ;; Define the size of the Sudoku grid

;; Define a structure to represent a Sudoku cell
(defstruct cell
  row col found value possible-values)  ;; Each cell has a row, column, found status, value, and possible values

;; Initialize the Sudoku grid
(defun initialize-sudoku (puzzle)
  "Creates a list of lists representing the Sudoku grid with constraint propagation."
  (mapcar #'(lambda (row r)
              (mapcar #'(lambda (value c)
                         (if (= value 0)  ;; If the cell is empty, set possible values from 1-9
                             (make-cell :row r :col c :found nil :value nil :possible-values '(1 2 3 4 5 6 7 8 9))
                             (make-cell :row r :col c :found t :value value :possible-values '())))
                      row (loop for c from 0 to (- +size+ 1) collect c)))
          puzzle (loop for r from 0 to (- +size+ 1) collect r)))

;; Retrieve a specific cell from the grid
(defun get-cell (grid row col)
  (nth col (nth row grid)))

;; Set the value of a cell and mark it as found
(defun set-cell-value (grid row col value)
  (setf (cell-value (get-cell grid row col)) value)  ;; Assign the value to the cell
  (setf (cell-found (get-cell grid row col)) t)  ;; Mark the cell as found
  (setf (cell-possible-values (get-cell grid row col)) '()))  ;; Clear possible values

;; Eliminate impossible values based on constraints
(defun eliminate (grid)
  "Eliminates impossible values from each cell based on Sudoku constraints."
  (loop for row from 0 below +size+ do
        (loop for col from 0 below +size+ do
              (let ((cell (get-cell grid row col)))
                (when (not (cell-found cell))
                  (let ((non-possible-values (get-non-possible-values grid row col)))
                    (setf (cell-possible-values cell)
                          (set-difference (cell-possible-values cell) non-possible-values))))))))

;; Get values that are already present in the row, column, and 3x3 box
(defun get-non-possible-values (grid row col)
  "Returns a list of numbers already present in the row, column, and 3x3 box."
  (let ((values '()))
    ;; Collect row values
    (loop for c from 0 below +size+ do
          (let ((cell (get-cell grid row c)))
            (when (cell-found cell)
              (pushnew (cell-value cell) values))))
    ;; Collect column values
    (loop for r from 0 below +size+ do
          (let ((cell (get-cell grid r col)))
            (when (cell-found cell)
              (pushnew (cell-value cell) values))))
    ;; Collect box values
    (let* ((box-start-row (* 3 (floor row 3)))
           (box-start-col (* 3 (floor col 3))))
      (loop for r from box-start-row below (+ box-start-row 3) do
            (loop for c from box-start-col below (+ box-start-col 3) do
                  (let ((cell (get-cell grid r c)))
                    (when (cell-found cell)
                      (pushnew (cell-value cell) values)))))))
    values))

;; Solve using constraint propagation
(defun constraint-solve (grid)
  "Solves the Sudoku puzzle using Constraint Propagation."
  (loop until (is-solved grid) do
        (eliminate grid)
        (apply-single-value-rule grid)
        (apply-only-choice-rule grid)))

;; Apply the rule where a cell has only one possible value left
(defun apply-single-value-rule (grid)
  "If any cell has only one possible value, set it."
  (loop for row from 0 below +size+ do
        (loop for col from 0 below +size+ do
              (let ((cell (get-cell grid row col)))
                (when (and (not (cell-found cell)) (= (length (cell-possible-values cell)) 1))
                  (set-cell-value grid row col (car (cell-possible-values cell))))))))

;; Apply the rule where a number can only appear in one position in a row, column, or box
(defun apply-only-choice-rule (grid)
  "Finds a number that can only appear in one cell of a row, column, or box."
  ;; Apply for rows
  (loop for row from 0 below +size+ do
        (loop for num from 1 to +size+ do
              (let ((possible-cells '()))
                (loop for col from 0 below +size+ do
                      (let ((cell (get-cell grid row col)))
                        (when (and (not (cell-found cell)) (member num (cell-possible-values cell)))
                          (push col possible-cells))))
                (when (= (length possible-cells) 1)
                  (set-cell-value grid row (car possible-cells) num)))))
  ;; Apply for columns
  (loop for col from 0 below +size+ do
        (loop for num from 1 to +size+ do
              (let ((possible-cells '()))
                (loop for row from 0 below +size+ do
                      (let ((cell (get-cell grid row col)))
                        (when (and (not (cell-found cell)) (member num (cell-possible-values cell)))
                          (push row possible-cells))))
                (when (= (length possible-cells) 1)
                  (set-cell-value grid (car possible-cells) col num))))))

;; Check if the Sudoku is completely solved
(defun is-solved (grid)
  "Checks if the Sudoku is completely solved."
  (loop for row from 0 below +size+ always
        (loop for col from 0 below +size+ always
              (cell-found (get-cell grid row col)))))

;; Print the Sudoku grid in a readable format
(defun print-sudoku (grid)
  "Prints the Sudoku grid in a readable format."
  (loop for row from 0 below +size+ do
        (loop for col from 0 below +size+ do
              (format t "~D " (cell-value (get-cell grid row col))))
        (format t "~%")))

;; Solve the Sudoku
(let ((grid (initialize-sudoku *example-puzzle*)))
  (constraint-solve grid)
  (print-sudoku grid))
