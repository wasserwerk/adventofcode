;;;; 2022, Day 4: Camp Cleanup

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (split-string (buffer-string) "\n")))

(defun prepare-data (data)
  (mapcar (lambda (r)
            (destructuring-bind (a-1 a-2 b-1 b-2)
                (mapcar 'string-to-number (split-string r "[,-]"))
              (list (cons a-1 a-2) (cons b-1 b-2))))
          data))

(defun up-contains-p (pairs)
  (destructuring-bind ((a-1 . a-2) (b-1 . b-2))
      pairs
    (and (>= b-1 a-1)
         (<= b-2 a-2))))

(defun down-contains-p (pairs)
  (destructuring-bind ((a-1 . a-2) (b-1 . b-2))
      pairs
    (and (>= a-1 b-1)
         (<= a-2 b-2))))

(defun fully-contains-p (pairs)
  (or (up-contains-p pairs)
      (down-contains-p pairs)))

(defun overlap-p (pairs)
  (destructuring-bind ((a-1 . a-2) (b-1 . b-2))
      pairs
    (and (>= b-2 a-1)
         (>= a-2 b-1))))

(defun solve-1 (data)
  (count t (mapcar 'fully-contains-p data)))

(defun solve-2 (data)
  (count t (mapcar 'overlap-p data)))

(setq data (prepare-data (read-data "04.in")))

(solve-1 data)
(solve-2 data)
 
