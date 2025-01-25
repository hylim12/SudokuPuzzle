(defun calculate-entropy (dataset)
  "Computes the entropy of the dataset to evaluate splitting criteria."
  (let ((freqs (make-hash-table))
        (total (length dataset)))
    (loop for (puzzle . solution) in dataset do
      (incf (gethash solution freqs 0)))
    (loop for v being the hash-values in freqs summing
          (- (/ v total) (* (/ v total) (log (/ v total) 2))))))

(defun split-dataset (dataset feature-index threshold)
  "Splits the dataset based on a feature index and threshold value."
  (let ((left nil) (right nil))
    (loop for (puzzle . solution) in dataset do
      (if (<= (aref puzzle 0 feature-index) threshold)
          (push (cons puzzle solution) left)
          (push (cons puzzle solution) right)))
    (values left right)))

(defun build-tree (dataset &key (max-depth 10) (min-samples 5))
  "Builds a decision tree with hyperparameter tuning."
  (if (or (null dataset) (<= max-depth 0) (< (length dataset) min-samples))
      (list :leaf (calculate-entropy dataset))
      (multiple-value-bind (left right)
          (split-dataset dataset (random 9) (random 9))
        (list :split (random 9)
              :left (build-tree left :max-depth (1- max-depth) :min-samples min-samples)
              :right (build-tree right :max-depth (1- max-depth) :min-samples min-samples)))))

(defun tune-hyperparameters (dataset)
  "Performs hyperparameter tuning using different values for max-depth and min-samples."
  (loop for depth in '(5 10 15)
        maximizing (let ((tree (build-tree dataset :max-depth depth :min-samples 10)))
                     (calculate-entropy dataset))
        into best-depth
        finally (return best-depth)))

(defun test-decision-tree (filename)
  "Loads Sudoku dataset, builds a decision tree, and tunes hyperparameters."
  (let ((dataset (sudoku-hyperparameters::read-sudoku-dataset filename)))  ; <-- Fully qualified function call
    (format t "Optimal max-depth: ~a~%" (tune-hyperparameters dataset))
    (let ((tree (build-tree dataset :max-depth 10 :min-samples 5)))
      (format t "Decision Tree Structure: ~a~%" tree))))