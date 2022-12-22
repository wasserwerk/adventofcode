;;;; 2022, Day 21: Monkey Math

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
      (anaphoric-while (re-search-forward "^\\(.+?\\): " nil t)
        (setf (symbol-function (intern (match-string 1)))
              (get-expression)))))

(defun expression-const-p ()
  (if (looking-at "\\([0-9]+\\)")
      (match-string 1)))

(defun expression-add-p ()
  (if (looking-at "\\(.+\\) \\+ \\(.+\\)")
      (list (match-string 1) (match-string 2))))

(defun expression-sub-p ()
  (if (looking-at "\\(.+\\) \\- \\(.+\\)")
      (list (match-string 1) (match-string 2))))

(defun expression-mul-p ()
  (if (looking-at "\\(.+\\) \\* \\(.+\\)")
      (list (match-string 1) (match-string 2))))

(defun expression-div-p ()
  (if (looking-at "\\(.+\\) / \\(.+\\)")
      (list (match-string 1) (match-string 2))))

(defun get-expression ()
  (anaphoric-cond ((expression-const-p)
                   (lambda () (string-to-number it)))
                  ((expression-add-p)
                   (lambda () (+ (funcall (intern (car it)))
                                 (funcall (intern (cadr it))))))
                  ((expression-sub-p)
                   (lambda () (- (funcall (intern (car it)))
                                 (funcall (intern (cadr it))))))
                  ((expression-mul-p)
                   (lambda () (* (funcall (intern (car it)))
                                 (funcall (intern (cadr it))))))
                  ((expression-div-p)
                   (lambda () (/ (funcall (intern (car it)))
                                 (funcall (intern (cadr it))))))))

(defun solve-1 ()
  (root))

(setq data (read-data "21.in"))

(solve-1)
