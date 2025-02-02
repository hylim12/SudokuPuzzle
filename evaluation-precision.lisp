(defun evaluate-precision (algorithm puzzles solutions)
  "Evaluates precision by comparing the number of steps taken to the optimal number of steps."
  (let ((total-precision 0)
        (count 0))
    (dolist (puzzle puzzles)
      (let* ((computed (funcall algorithm puzzle))
             (correct (gethash puzzle solutions))
             (computed-steps (length (trace-search-path computed)))
             (optimal-steps (length (trace-search-path correct))))
        (incf total-precision (/ optimal-steps computed-steps))
        (incf count)))
    (/ total-precision count))) ;; Average precision score