;; 2022, Day 9: Rope Bridge

(defvar rope ())

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

(defun init (length)
  (setq tail-track ())
  (setq rope (loop repeat length
                   collect (cons 0 0)))
  rope)

(defun update-tail-track ()
  (add-to-list 'tail-track
               (copy-list (car (last rope)))))

(defun move-head (dir head)
  (case dir
    ('L (decf (car head)))
    ('R (incf (car head)))
    ('U (incf (cdr head)))
    ('D (decf (cdr head))))
  head)

(defun move-vector (a b)
  (cons (- (car a)
           (car b))
        (- (cdr a)
           (cdr b))))

(defun max-2 (n)
  (cond ((>= n 2)
         1)
        ((<= n -2)
         -1)
        (t n)))

(defun bring-it-closer (p)
  (cons (max-2 (car p))
        (max-2 (cdr p))))

(defun too-far-p (m)
  (or (> (abs (car m)) 1)
      (> (abs (cdr m)) 1)))

(defun reduce-move-vector (p m)
  (let ((n (bring-it-closer m)))
    (cons (+ (car p) (car n))
          (+ (cdr p) (cdr n)))))

(defun move-tail (a b)
  (let ((m (move-vector a b)))
    (if (too-far-p m)
        (reduce-move-vector b m)
      b)))

(defun process-tail (h r)
  (if (car r)
      (let ((v (move-tail h (car r))))
        (cons v
              (process-tail v (cdr r))))))

(defun move-rope (move)
  (loop repeat (cdr move)
        do (move-head (car move) (car rope))
        do (setq rope (append (list (car rope))
                              (process-tail (car rope) (cdr rope))))
        do (update-tail-track))
  rope)

(defun solve (length moves)
  (init length)
  (mapcar 'move-rope moves)
  (length tail-track))

(defun solve-1 (moves)
  (solve 2 moves))

(defun solve-2 (moves)
  (solve 10 moves))

(setq moves (read-data "09.in"))

(solve-1 moves)
(solve-2 moves)

