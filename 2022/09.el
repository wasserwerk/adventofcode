;; 2022, Day 9: Rope Bridge

(defvar rope '((0 . 0) (0 . 0)))

(defvar tail-track ())

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((pairs ()))
      (ignore-error end-of-file
        (while (not (eobp))
          (push (cons (read (current-buffer)) (read (current-buffer))) pairs)))
      (reverse pairs))))

(defun init ()
  (setq tail-track ())
  (setq rope (list (cons 0 0) (cons 0 0)))
  rope)

(defun tail-too-far-p ()
  (or (> (abs (- (caar rope)
                 (caadr rope)))
         1)
      (> (abs (- (cdar rope)
                 (cdadr rope)))
         1)))

(defun move (move)
  (loop repeat (cdr move)
        do
        (let ((head (copy-list (car rope))))
          (case (car move)
            ('L (decf (caar rope)))
            ('R (incf (caar rope)))
            ('U (incf (cdar rope)))
            ('D (decf (cdar rope))))
          (if (tail-too-far-p)
              (setf (cadr rope)
                    head)))
        (update-tail-track))
  rope)

(defun update-tail-track ()
  (add-to-list 'tail-track (copy-list (cadr rope))))

(defun solve-1 (moves)
  (init)
  (mapcar 'move moves)
  (length tail-track))

(setq moves (read-data "09.in"))

(solve-1 moves)
