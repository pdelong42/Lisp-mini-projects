#!/usr/bin/clisp

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

;; defined iteratively
;
;(defun prime?
;   (x)
;   (do
;      ((i 2 (+ i 1)))
;      ((> i (midpoint x)))
;      (if
;         (equal 0 (mod x i))
;         (return-from prime?)
;      )
;   )
;   t
;)

; defined recursively

(defun prime?
   (x)
   (labels
      (  (subrange
            (y)
            (unless
               (> y 1)
               (return-from prime? t)
            )
            (unless
               (> (mod x y) 0)
               (return-from prime?)
            )
            (subrange (- y 1))
         )
      )
      (subrange (midpoint x))
   )
)

;; defined iteratively
;
;(defun factors
;   (x)
;   (if
;      (prime? x)
;      (return-from factors (list x))
;   )
;   (do
;      ((i 2 (+ i 1)))
;      ((>= i x))
;      (unless
;         (> (mod x i) 0)
;         (return-from factors (cons i (factors (/ x i))))
;      )
;   )
;)

; defined recursively

(defun factors
   (x)
   (if
      (prime? x)
      (return-from factors (list x))
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
               (return-from factors (cons y (factors (/ x y))))
            )
            (nexttry (+ y 1))
         )
      )
      (nexttry 2)
   )
)

;(format t "~A~%" (even? (read)))
;(format t "~A~%" (prime? (read)))

(do
   ((i 1 (+ i 1)))
   ((>= i 100))
   (format t "~A ~A~%" i (factors i))
)
