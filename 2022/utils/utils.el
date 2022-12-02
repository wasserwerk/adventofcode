(defun read-lines (file)
  "Read file and return a list of all lines as strings."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((lines ()))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
        (forward-line 1))
      (reverse lines))))

(defun read-lines-as-numbers (file)
  "Read file and return a list of all lines as numbers."
  (mapcar 'string-to-number (read-lines file)))

(defun read-pairs (file)
  "Read file as pairs of two symbols and return a list of cons."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((pairs ()))
      (ignore-error end-of-file
        (while (not (eobp))
          (push (cons (read (current-buffer)) (read (current-buffer))) pairs)))
      (reverse pairs))))
