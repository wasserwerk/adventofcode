;;;; 2022, Day 2: Rock Paper Scissors

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((pairs ()))
      (ignore-error end-of-file
        (while (not (eobp))
          (push (cons (read (current-buffer)) (read (current-buffer))) pairs)))
      (reverse pairs))))

(setq strategy-guide
      '((A . Y)
        (B . X)
        (C . Z)))

(setq score-shape
      '((X . 1)
        (Y . 2)
        (Z . 3)))

(setq score-indicator
      '((X . 0)
        (Y . 3)
        (Z . 6)))

(setq score-outcome
      '(((A . X) . 3)
        ((A . Y) . 6)
        ((A . Z) . 0)
        ((B . X) . 0)
        ((B . Y) . 3)
        ((B . Z) . 6)
        ((C . X) . 6)
        ((C . Y) . 0)
        ((C . Z) . 3)))

(defun score-1 (round)
  (+ (cdr (assoc (cdr round) score-shape))
     (cdr (assoc round score-outcome))))

(defun score-2 (round)
  (let ((outcome (find-if (lambda (o)
                            (and (eq (car round)
                                     (caar o))
                                 (eq (cdr (assoc (cdr round) score-indicator))
                                     (cdr o))))
                          score-outcome)))
    (+ (cdr outcome)
       (cdr (assoc (cdar outcome) score-shape)))))

(defun solve-1 (data)
  (apply '+ (mapcar 'score-1 data)))

(defun solve-2 (data)
  (apply '+ (mapcar 'score-2 data)))

(setq data (read-data "02.in"))

(solve-1 data)
(solve-2 data)
