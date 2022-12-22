;;;; 2022, Day 3: Rucksack Reorganization

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (split-string (buffer-string) "\n")))

(defun part-1 (rucksack)
  (apply 'seq-intersection
         (mapcar (lambda (s)
                   (split-string s "" t))
                 (seq-partition rucksack (/ (length rucksack) 2)))))

(defun fn-1 (data)
  (mapcar 'part-1 data))

(defun part-2 (rucksack-group)
  (reduce 'seq-intersection
          (mapcar (lambda (s)
                    (split-string s "" t))
                  rucksack-group)))

(defun fn-2 (data)
  (mapcar 'part-2 (seq-partition data 3)))

(defun solve (fn data)
  (apply '+ (mapcar (lambda (c)
                      (let ((code (string-to-char c)))
                        (cond ((s-uppercase-p c)
                               (- code 38))
                              ((s-lowercase-p c)
                               (- code 96))
                              (t 0))))
                    (apply 'append (mapcar (lambda (i)
                                             (or (remove-duplicates i :test 'string=) ""))
                                           (funcall fn data))))))

(setq data (read-lines "03.in"))

(solve 'fn-1 data)
(solve 'fn-2 data)
