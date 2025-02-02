(defun measure-time (function &rest args)
  "Measures the execution time of a function in milliseconds."
  (let ((start (get-internal-real-time)))
    (apply function args)
    (/ (- (get-internal-real-time) start) 1000.0))) ;; Convert to milliseconds