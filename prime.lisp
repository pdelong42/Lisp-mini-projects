(defun even?
   (x)
   (equal (mod x 2) 0)
)

(defun midpoint
   (x)
   (if
      (even? x)
      (/ x 2)
      (/ (- x 1) 2)
   )
)

(defun prime?-iterative
   (x)
   (do
      (  (i 2 (+ i 1)))
      (  (> i (midpoint x)))
      (if
         (equal 0 (mod x i))
         (return-from prime?-iterative)
      )
   )
   t
)

(defun prime?-recursive
   (x)
   (labels
      (  (subrange
            (y)
            (unless
               (> y 1)
               (return-from prime?-recursive t)
            )
            (unless
               (> (mod x y) 0)
               (return-from prime?-recursive)
            )
            (subrange (- y 1))
         )
      )
      (subrange (midpoint x))
   )
)

(defun factors-iterative
   (x)
   (if
      (prime? x)
      (return-from factors-iterative (list x))
   )
   (do
      (  (i 2 (+ i 1)))
      (  (>= i x))
      (unless
         (> (mod x i) 0)
         (return-from factors-iterative (cons i (factors (/ x i))))
      )
   )
)

(defun factors-recursive
   (x)
   (if
      (prime? x)
      (return-from factors-recursive (list x))
   )
   (labels
      (  (nexttry
            (y)
            (unless
               (< y x)
               (return-from nexttry)
            )
            (unless
               (> (mod x y) 0)
               (return-from factors-recursive (cons y (factors (/ x y))))
            )
            (nexttry (+ y 1))
         )
      )
      (nexttry 2)
   )
)

(defun prime?  (x) (prime?-iterative  x))
(defun factors (x) (factors-iterative x))

;(format t "~A~%" (even? (read)))
;(format t "~A~%" (prime? (read)))

;(do
;   ((i 1 (+ i 1)))
;   ((>= i 100))
;   (format t "~A ~A~%" i (factors i))
;)
