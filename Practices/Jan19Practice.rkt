#lang racket

;;;; Scheme Function Practice

;; Takes two numbers (x, y) and returns x raised to power y
(define pow
  (lambda (x y) ; put inputs in parentheses!
    (if (eq? y 0)
        1
        (* x (pow x (- y 1))))))

;; Takes two positive ints and returns the GCD of two integers
;; Use the Euclid's algorithm
(define mygcd
  (lambda (x y)
    (if (zero? y)
         x
         (mygcd y (modulo x y)))))