#lang racket

; doubles the input
(define times2
    (lambda (x)
        (* x 2)))

; factorial
(define factorial
    (lambda (x)
        (if (zero? x)
            1
            (* x (factorial (- x 1))))))

; pow takes 2 (non-negative) numbers x and y & output x^y
(define pow
    (lambda (x y)
        (if (zero? y)
            1
            (* x (pow x (- y 1))))))

; gcd takes 2 (non-neg) numbers x and y and returns their greatest common divisor
; gcd(a, b) = gcd(b, a remainder b)
(define gcd
    (lambda (a b)
        (if (zero? b)
            a
            (gcd b(remainder a b)))))