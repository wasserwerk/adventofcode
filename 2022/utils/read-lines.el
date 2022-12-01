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

