;;;; 2022, Day 10: Cathode-Ray Tube

(defun read-data (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (prog)
      (while (not (eobp))
        (push (buffer-substring (line-beginning-position) (line-end-position)) prog)
        (forward-line))
      (reverse prog))))

(defun decode (inst)
  (cond ((string-match "addx[ +]\\([-]*[0-9]+\\)" inst)
         (cons 2 (string-to-number (substring-no-properties inst (match-beginning 1) (match-end 1)))))
        ((string-match "noop" inst)
         (cons 1 0))
        (t (error "invalid instruction"))))

(defun solve-1 (data)
  (loop with prog = data
        with cycles = '(20 60 100 140 180 220)
        with cycle = (pop cycles)
        with reg = 1
        with pc = 0
        for inst = (decode (pop prog))
        while (and inst cycle)
        if (and (>= pc (- cycle 2))
                (<= pc cycle))
        sum (* cycle reg)
        and do (setq cycle (pop cycles))
        do (incf pc (car inst))
        do (incf reg (cdr inst))))

(defun draw-pixel-p (p s)
  (and (>= p s)
       (<= p (+ s 2))))

(defmacro update-pixel (p s r)
  `(if (draw-pixel-p ,p ,s)
       (push 1 ,r)
     (push 0 ,r)))

(defun solve-2 (data)
  (let ((display (loop with reg = 1
                       with sprite = (1- reg)
                       with pos = 0
                       with cycle = 1
                       with code = ()
                       with row = ()
                       for inst in data
                       while inst
                       do (setq code (decode inst))
                       do (loop with c = 0
                                repeat (car code)
                                do (update-pixel pos sprite row)
                                do (incf pos)
                                if (and (= c 0)
                                        (= (car code) 2))
                                do (incf cycle)
                                do (incf c)
                                if (= pos 40)
                                do (setq pos 0))
                       do (incf reg (cdr code))
                       do (setq sprite (1- reg))
                       do (incf cycle)
                       finally do (return (reverse row)))))
    (progn
      (princ (concat (string-join 
                      (mapcar (lambda (row)
                                (apply 'string (subst ?. 0 (subst ?# 1 row))))
                              (seq-partition display 40))
                      "\n") "\n"))
      nil)))

(setq data (read-data "10.in"))


(solve-1 data)
(solve-2 data)
