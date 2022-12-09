;;;; 2022, Day 8: Treetop Tree House

;; Needs dash for flatten
(require 'dash)

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

(defun part-1 (seq)
  (let ((rr (reduce-with-intermediates 'max seq))
        (rl (reverse (reduce-with-intermediates 'max (reverse seq)))))
    (append '(t)
            (cl-subseq (cl-mapcar (lambda (a b)
                                    (or a b))
                                  (cl-mapcar '>
                                             rr
                                             (rotate-list-right rr))
                                  (cl-mapcar '>
                                             rl
                                             (rotate-list-left rl)))
                       1 -1)
            '(t))))

(defun solve-1 (data)
  (count t (-flatten (cl-mapcar (lambda (seq1 seq2)
                                 (cl-mapcar (lambda (a b)
                                              (or a b))
                                            seq1
                                            seq2))
                               (mapcar 'part-1 data)
                               (transpose-matrix (mapcar
                                                  'part-1
                                                  (transpose-matrix data)))))))

(defun count-until-aux (pred elm seq counter)
  (if seq
      (progn
        (incf counter)
        (if (not (funcall pred (car seq) elm))
            (count-until-aux pred elm (cdr seq) counter)
          counter))
    counter))

(defun count-until (pred elm seq)
  (count-until-aux pred elm seq 0))

(defun count-trees (data)
  (mapcar (lambda (seq)
            (maplist (lambda (sub)
                       (count-until '>= (car sub) (cdr sub)))
                     seq))
          data))

(defun solve-2 (data)
  (apply 'max (-flatten
               (cl-mapcar (lambda (a b c d)
                            (cl-mapcar '* a b c d))
                          (count-trees data)
                          (mapcar 'reverse
                                  (count-trees (mapcar 'reverse data)))
                          (transpose-matrix (count-trees (transpose-matrix data)))
                          (transpose-matrix (mapcar
                                             'reverse
                                             (count-trees (mapcar
                                                           'reverse
                                                           (transpose-matrix data)))))))))

(setq data (prepare-data (read-data "08.in")))

(solve-1 data)
(solve-2 data)
