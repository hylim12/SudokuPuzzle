;; Function to check if the computed solution matches the correct one
(defun compare-solutions (computed-solution correct-solution)
  "Checks if the computed solution matches the correct solution."
  (equal computed-solution correct-solution))

;; Function to evaluate accuracy
(defun evaluate-accuracy (algorithm puzzles solutions)
  "Evaluates the accuracy of the search algorithm."
  (let ((correct-count 0)
        (total (length puzzles)))
    (dolist (puzzle puzzles)
      (let ((computed (funcall algorithm puzzle))
            (correct (gethash puzzle solutions))) ;; Retrieve expected solution
        (if (compare-solutions computed correct)
            (incf correct-count))))
    (* (/ correct-count total) 100))) ;; Calculate accuracy percentage


