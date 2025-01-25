;;GridSearch CV
(defun cartesian-product (lists)
  "Generate all possible combinations of values from multiple lists."
  (if (null lists)
      '(())
      (let ((rest (cartesian-product (cdr lists))))
        (apply 'append (mapcar (lambda (x)
                                 (mapcar (lambda (y) (cons x y)) rest))
                               (car lists))))))

(defun grid-search (param-grid data labels test-data test-labels)
  "Performs GridSearch over a parameter grid."
  (let ((combinations (cartesian-product (mapcar #'cdr param-grid)))
        (best-score -1)
        (best-params nil))
    (dolist (combo combinations)
      (let ((params (mapcar #'cons (mapcar #'car param-grid) combo)))
        (let ((score (train-and-evaluate data labels test-data test-labels
                                         (cdr (assoc 'max-depth params))
                                         (cdr (assoc 'min-samples params)))))
          (when (> score best-score)
            (setf best-score score
                  best-params params)))))
    (list best-params best-score)))

;; RandomSearch CV
(defun random-sample (list)
  "Select a random element from a list."
  (nth (random (length list)) list))

(defun random-search (param-grid data labels test-data test-labels num-trials)
  "Performs RandomSearch for hyperparameter tuning."
  (let ((best-score -1)
        (best-params nil))
    (dotimes (i num-trials)
      (let ((params (mapcar (lambda (p) (cons (car p) (random-sample (cdr p)))) param-grid)))
        (let ((score (train-and-evaluate data labels test-data test-labels
                                         (cdr (assoc 'max-depth params))
                                         (cdr (assoc 'min-samples params)))))
          (when (> score best-score)
            (setf best-score score
                  best-params params)))))
    (list best-params best-score)))
