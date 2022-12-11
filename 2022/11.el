;;;; 2022, Day 11: Monkey in the Middle

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (data
          record)
      (while (not (eobp))
        (setq record ())
        (when (re-search-forward "Monkey \\(.+?\\):" nil t)
          (push :monkey record)
          (push (string-to-number (match-string 1)) record))
        (when (re-search-forward "Starting items: \\(.+\\)" nil t)
          (push :item record)
          (push (string-to-number-list (match-string 1)) record))
        (when (re-search-forward "Operation: new = \\(.+\\)" nil t)
          (push :operation record)
          (push (match-string 1) record))
        (when (re-search-forward "Test: divisible by \\(.+\\)" nil t)
          (push :test record)
          (push (string-to-number (match-string 1)) record))
        (when (re-search-forward "If true: throw to monkey \\(.+\\)" nil t)
          (push :true-condition record)
          (push (string-to-number (match-string 1)) record))
        (when (re-search-forward "If false: throw to monkey \\(.+\\)" nil t)
          (push :false-condition record)
          (push (string-to-number (match-string 1)) record))
        (push :inspection-counter record)
        (push 0 record)
        (push (reverse record) data))
      (reverse data))))

(defun string-to-number-list (str)
  (mapcar 'string-to-number (split-string str ", ")))

(defun eval-operation (expr arg)
  (string-to-number
   (calc-eval
    (replace-regexp-in-string "old" (number-to-string arg) expr))))

(defun get-monkey (number)
  (find-if (lambda (a)
             (= (getf a :monkey) number))
           data))

(defun pop-item (monkey)
  (pop (getf monkey :item)))

(defun get-monkey-number (monkey)
  (getf monkey :monkey))

(defun get-item (monkey)
  (getf monkey :item))

(defun get-operation (monkey)
  (getf monkey :operation))

(defun get-test (monkey)
  (getf monkey :test))

(defun get-true-condition (monkey)
  (getf monkey :true-condition))

(defun get-false-condition (monkey)
  (getf monkey :false-condition))

(defun get-inspection-counter (monkey)
  (getf monkey :inspection-counter))

(defun update-inspection-counter (monkey)
  (incf (getf monkey :inspection-counter)))

(defun add-item (monkey-number item)
  (let ((monkey (get-monkey monkey-number)))
    (setf (getf monkey :item)
          (append (getf monkey :item) (list item)))))

(defun inspect-item (monkey)
  (let ((worry-level (pop-item monkey)))
    (if worry-level
        (let* ((new-worry-level (eval-operation (get-operation monkey) worry-level))
               (borred-worry-level (/ new-worry-level 3))
               (div-p (= (mod borred-worry-level (get-test monkey)) 0))
               (throw-to (if div-p (get-true-condition monkey) (get-false-condition monkey))))
          (update-inspection-counter monkey)
          (add-item throw-to borred-worry-level)))
    worry-level))

(defun process-monkey (monkey)
  (while (inspect-item monkey)))

(defun process-all-monkey ()
  (mapcar 'process-monkey data))

(defun solve-1 ()
  (loop repeat 20
        do (process-all-monkey))
  (let ((counter (sort (mapcar 'get-inspection-counter data) '>)))
    (* (car counter) (cadr counter))))

(setq data (read-data "11.in"))

(solve-1)
