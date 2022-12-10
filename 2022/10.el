;;;; 2022, Day 10: Cathode-Ray Tube

(defvar cycles '(20 60 100 140 180 220))

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (prog)
      (while (not (eobp))
        (cond ((looking-at "^addx \\([-]*[0-9]+\\)$")
               (push (cons 2 (string-to-number (match-string 1))) prog))
              ((looking-at "^noop$")
               (push (cons 1 0) prog)))
        (forward-line))
      (reverse prog))))

(cl-defun reduce-with-intermediates (fn xs &optional carry)
  "Source: https://kaygun.tumblr.com/post/661067146753982464/reduce-with-intermediate-results-in-common-lisp"
  (if (null xs)
      (nreverse carry)
      (let ((res (if (null carry)
                     (funcall fn (car xs))
                     (funcall fn (car xs) (car carry)))))
        (reduce-with-intermediates fn (cdr xs) (cons res carry)))))

(defun run-program (prog)
  (reduce-with-intermediates (lambda (a &optional b)
                               (cons (+ (car a) (or (car b) 0))
                                     (+ (cdr a) (or (cdr b) 0))))
                             (append (list '(0 . 1)) prog)))

(defun cycle-value (run cycle)
  (car (last (-take-while (lambda (p)
                            (<= (car p) (1- cycle)))
                          run))))

(defun cycle-values (run)
  (mapcar (lambda (c)
            (cycle-value run c))
          cycles))

(defun solve-1 (run)
  (apply '+
         (cl-mapcar (lambda (c v)
                      (* c (cdr v)))
                    cycles
                    (cycle-values run))))

(setq prog (read-data "10.in"))

(solve-1 (run-program prog))

