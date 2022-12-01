(defun read-list (file)
  "Read file and return a list of all lines as strings."
  (split-string
   (with-temp-buffer
     (insert-file-contents file)
     (goto-char (point-min))
     (buffer-string))
   "\n"))

(defun read-number-list (file)
  "Read file and return a list of all lines as numbers."
  (mapcar 'string-to-number (read-list file)))

