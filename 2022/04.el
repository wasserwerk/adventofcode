;;;; 2022, Day 4: Camp Cleanup

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (split-string (buffer-string) "\n")))

(defun prepare-data (data)
  (mapcar (lambda (r)
            (destructuring-bind (a1 a2 b1 b2)
                (mapcar 'string-to-number (split-string r "[,-]"))
              (list (cons a1 a2) (cons b1 b2))))
          data))

(defun fully-contains-p (pairs)
  (destructuring-bind ((a1 . a2) (b1 . b2))
      pairs
    (or (and (>= b1 a1)
             (<= b2 a2))
        (and (>= a1 b1)
             (<= a2 b2)))))

(defun overlap-p (pairs)
  (destructuring-bind ((a1 . a2) (b1 . b2))
      pairs
    (and (>= b2 a1)
         (>= a2 b1))))

(defun solve-1 (data)
  (count t (mapcar 'fully-contains-p data)))

(defun solve-2 (data)
  (count t (mapcar 'overlap-p data)))

(setq data (prepare-data (read-data "04.in")))

(solve-1 data)
(solve-2 data)

