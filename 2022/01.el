;; You need dash.el for partition. 
(require 'dash)

(load-file "utils/read-lines.el")

(defun solve (cals)
  (apply 'max
         (mapcar (lambda (x)
                   (apply '+ x))
                 cals)))

(solve (-split-when 'zerop (read-lines-as-numbers "01.in")))

