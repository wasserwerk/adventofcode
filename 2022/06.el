;;;; 2022, Day 6: Tuning Trouble

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (buffer-string)))

(defun repeat-p (seq)
  (if (cdr seq)
      (if (member (car seq) (cdr seq))
          (cdr seq)
        (repeat-p (cdr seq)))))

(defun find-marker-position (seq len)
  (loop with offset = 0
        for rest on (split-string seq "" t)
        until (= (length rest) len)
        while (repeat-p (seq-take rest len))
        finally (return (+ offset len))
        do (incf offset)))

(defun solve-1 (seq)
  (find-marker-position seq 4))

(defun solve-2 (seq)
  (find-marker-position seq 14))

(setq data (read-data "06.in"))

(solve-1 data)
(solve-2 data)
