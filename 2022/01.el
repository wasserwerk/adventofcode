;;;; Day 1: Calorie Counting

;; Need dash for partition and take.
(require 'dash)

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (split-string (buffer-string) "\n")))

(defun read-lines-as-numbers (file)
  (mapcar 'string-to-number (read-lines file)))

(defun prepare-data (data)
  (setq calories (-split-when 'zerop data)))

(defun sum-calories (calories)
  (mapcar (lambda (x)
            (apply '+ x))
          calories))

(defun solve-1 (calories)
  (apply 'max (sum-calories calories)))

(defun solve-2 (calories)
  (apply '+ (seq-take (sort (mapcar (lambda (x)
                                      (apply '+ x))
                                    calories)
                            '>) 3)))

(setq data (prepare-data (read-lines-as-numbers "01.in")))

(solve-1 data)
(solve-2 data)
