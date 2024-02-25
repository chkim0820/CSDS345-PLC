#lang racket

; multiply a list of numbers & immediately return on a 0; use normal recursion
(define multiply
  (lambda (lis)
    (call/cc (lambda (break) (multiply-break lis break)))))

(define multiply-break
  (lambda (lis break)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) (break 0))
      (else (* (car lis) (multiply-break (cdr lis) break))))))