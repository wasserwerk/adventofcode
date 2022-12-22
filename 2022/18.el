;;;; 2022, Day 18: Boiling Boulders

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (data)
      (while (re-search-forward "\\([0-9]+\\)" nil t)
        (push (string-to-number (match-string 1)) data))
      (reverse data))))

(defun cube-diff (cube1 cube2)
  (cl-mapcar (lambda (a b) (abs (- a b))) cube1 cube2))

(defun adjacent-cube-p (cube1 cube2)
  (let ((diff (cube-diff cube1 cube2)))
    (and (= (count 0 diff) 2)
         (= (count 1 diff) 1))))

(defun solve-1 (data)
  (loop with c = 0
        for rest on data
        do (loop for e in (cdr rest)
                 when (adjacent-cube-p (car rest) e)
                 do (incf c))
        finally (return (- (* 6 (length data)) (* 2 c)))))

(setq data (seq-partition (read-data "18.in") 3))

(solve-1 data)
