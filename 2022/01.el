;; Need dash.el for partition and take.
(require 'dash)

(load-file "utils/utils.el")

(defun sum-cals (cals)
  (mapcar (lambda (x)
            (apply '+ x))
          cals))

(setq cals (-split-when 'zerop (read-lines-as-numbers "01.in")))

;; part 1
(apply 'max (sum-cals cals))

;; part 2
(apply '+ (-take 3 (sort (mapcar (lambda (x)
                                   (apply '+ x))
                                 cals)
                         '>)))
