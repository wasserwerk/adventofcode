;;;; 2022, Day 8: Treetop Tree House

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (split-string (buffer-string) "\n")))

(defun prepare-data (data)
  (mapcar (lambda (s)
            (mapcar 'string-to-number
                    (split-string s "" t)))
          data))

(defun transpose-matrix (matrix)
  (apply 'cl-mapcar 'list matrix))

(cl-defun reduce-with-intermediates (fn xs &optional carry)
  "Source: https://kaygun.tumblr.com/post/661067146753982464/reduce-with-intermediate-results-in-common-lisp"
  (if (null xs)
      (nreverse carry)
    (let ((res (if (null carry)
                   (funcall fn (car xs))
                 (funcall fn (car xs) (car carry)))))
      (reduce-with-intermediates fn (cdr xs) (cons res carry)))))

(defun visible (seq)
  (let ((rr (reduce-with-intermediates 'max seq))
        (rl (reverse (reduce-with-intermediates 'max (reverse seq)))))
    (append '(t)
            (cl-mapcar (lambda (a b)
                         (or a b))
                       (cl-subseq (cl-mapcar '>
                                             rr
                                             (rotate-list-right rr))
                                  1 -1)
                       (cl-subseq (cl-mapcar '>
                                             rl
                                             (rotate-list-left rl))
                                  1 -1))
            '(t))))

(defun solve-1 (data)
  (count t (flatten (cl-mapcar (lambda (seq1 seq2)
                                 (cl-mapcar (lambda (a b)
                                              (or a b))
                                            seq1
                                            seq2))
                               (mapcar 'visible data)
                               (transpose-matrix (mapcar 'visible (transpose-matrix data)))))))

(setq data (prepare-data (read-data "08.in")))

(solve-1 data)
