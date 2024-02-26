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

; PRACTICE:
(define indexof
  (lambda (x lis)
    (call/cc (lambda (break) (indexof-break x lis break)))))

(define indexof-break
  (lambda (x lis break)
    (cond
      ((null? lis) (break -1))
      ((eq? x (car lis)) 0)
      (else (+ 1 (indexof-break x (cdr lis) break))))))