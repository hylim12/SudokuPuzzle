(defun split-dataset (data labels feature threshold)
  "Splits dataset based on a feature and threshold."
  (let ((left-data '()) (left-labels '()) (right-data '()) (right-labels '()))
    (loop for row in data and label in labels do
      (if (<= (nth feature row) threshold)
          (progn (push row left-data) (push label left-labels))
          (progn (push row right-data) (push label right-labels))))
    (values left-data left-labels right-data right-labels)))

(defun gini-impurity (labels)
  "Calculates Gini impurity of a dataset."
  (let ((total (length labels)))
    (if (zerop total)
        0
        (let* ((counts (mapcar (lambda (x) (/ (count x labels) (float total))) (remove-duplicates labels)))
               (sum (reduce #'+ (mapcar (lambda (p) (* p p)) counts))))
          (- 1 sum)))))

(defun best-split (data labels)
  "Finds the best feature and threshold to split the data."
  (let ((best-gini 1) (best-feature nil) (best-threshold nil))
    (loop for feature from 0 below (length (first data)) do
      (loop for row in data do
        (let ((threshold (nth feature row)))
          (multiple-value-bind (left-data left-labels right-data right-labels)
              (split-dataset data labels feature threshold)
            (let* ((left-gini (gini-impurity left-labels))
                   (right-gini (gini-impurity right-labels))
                   (gini (/ (+ (* (length left-labels) left-gini) (* (length right-labels) right-gini))
                            (length labels))))
              (when (< gini best-gini)
                (setf best-gini gini
                      best-feature feature
                      best-threshold threshold)))))))
    (values best-feature best-threshold)))

(defun build-tree (data labels max-depth min-samples)
  "Recursively builds the decision tree."
  (if (or (zerop (length data)) (<= (length labels) min-samples) (zerop max-depth))
      (make-node :value (majority-class labels))
      (multiple-value-bind (feature threshold) (best-split data labels)
        (if (null feature)
            (make-node :value (majority-class labels))
            (multiple-value-bind (left-data left-labels right-data right-labels)
                (split-dataset data labels feature threshold)
              (make-node :feature feature
                         :index threshold
                         :left (build-tree left-data left-labels (1- max-depth) min-samples)
                         :right (build-tree right-data right-labels (1- max-depth) min-samples)))))))

(defun predict (tree row)
  "Predicts the label for a single data point using the decision tree."
  (if (node-value tree)
      (node-value tree)
      (if (<= (nth (node-feature tree) row) (node-index tree))
          (predict (node-left tree) row)
          (predict (node-right tree) row))))

(defun evaluate (tree test-data test-labels)
  "Evaluates the tree using accuracy."
  (let ((correct 0) (total (length test-labels)))
    (loop for row in test-data and label in test-labels do
      (when (equal (predict tree row) label)
        (incf correct)))
    (/ correct (float total))))

(defun train-and-evaluate (data labels test-data test-labels max-depth min-samples)
  "Trains a decision tree and evaluates its accuracy."
  (let ((tree (build-tree data labels max-depth min-samples)))
    (evaluate tree test-data test-labels)))

;; GridSearch
(defun grid-search (param-grid data labels test-data test-labels)
  "Performs GridSearch over a parameter grid."
  (let ((best-score -1) (best-params nil))
    (dolist (max-depth '(3 5 7))
      (dolist (min-samples '(1 2 5))
        (let ((score (train-and-evaluate data labels test-data test-labels max-depth min-samples)))
          (when (> score best-score)
            (setf best-score score
                  best-params (list (cons 'max-depth max-depth) (cons 'min-samples min-samples)))))))
    (list best-params best-score)))

;; RandomSearch
(defun random-search (param-grid data labels test-data test-labels num-trials)
  "Performs RandomSearch for hyperparameter tuning."
  (let ((best-score -1) (best-params nil))
    (dotimes (i num-trials)
      (let* ((max-depth (nth (random 3) '(3 5 7)))
             (min-samples (nth (random 3) '(1 2 5)))
             (score (train-and-evaluate data labels test-data test-labels max-depth min-samples)))
        (when (> score best-score)
          (setf best-score score
                best-params (list (cons 'max-depth max-depth) (cons 'min-samples min-samples)))))))
    (list best-params best-score)))

(multiple-value-bind (puzzles solutions) (load-csv "sudoku.csv")
  (multiple-value-bind (data labels) (preprocess-data puzzles solutions)
    (format t "GridSearch Best: ~a~%" (grid-search nil data labels data labels))
    (format t "RandomSearch Best: ~a~%" (random-search nil data labels data labels 10))))
