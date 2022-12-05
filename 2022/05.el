(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (stack
          moves
          end)
      (save-excursion
        (setq end (re-search-forward "^$" nil t)))
      (while (re-search-forward "\\([A-Z]\\| [0-9] \\|    \\)" end t)
        (setq stack (append stack (list (match-string 1)))))
      (while (re-search-forward "\\([0-9]+\\)" nil t)
        (setq moves (append moves (list (match-string 1)))))
      (list stack moves))))

(defun number-of-stacks (stack)
  (string-to-number (car (last stack))))

(defun prepare-stack (stack)
  (setq number-of-stacks (number-of-stacks stack))
  (mapcar (lambda (s)
            (remove "    " s))
          (apply 'mapcar* 'list
                 (butlast (seq-partition stack number-of-stacks)))))

(defun prepare-moves (moves)
  (seq-partition (mapcar 'string-to-number moves) 3))

(defun move-1 (stack count from to)
  (dotimes (i count)
    (push (pop (nth (1- from) stack)) (nth (1- to) stack))))

(defun move-2 (stack count from to)
  (let (load)
    (dotimes (i count)
      (push (pop (nth (1- from) stack)) load))
    (dotimes (i count)
      (push (pop load) (nth (1- to) stack)))))

(defun top (stack)
  (string-join (mapcar 'car stack)))

(defun solve (file fn)
  (let* ((number-of-stacks 0)
         (data (read-data file))
         (stack (prepare-stack (nth 0 data)))
         (moves (prepare-moves (nth 1 data))))
    (mapc (lambda (m) (apply fn stack m)) moves)
    (top stack)))

(solve "05.in" 'move-1)
(solve "05.in" 'move-2)
