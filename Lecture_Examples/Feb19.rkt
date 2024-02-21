#lang racket

; Examples where we generalize the return

; split: splits a list into two lists; one with the even indeces and one with odd
; (split '(a b c d e)) => '((a c e) (b d))
(define split-orig
  (lambda (lis)
    (if (null? lis)
        '(() ())
        (cons (cons (car lis) (car (cdr (split-orig (cdr lis))))) (cons (car (split-orig (cdr lis))) '())))))

(define split-cps
  (lambda (lis return)
    (if (null? lis)
        (return '() '())
        (split-cps (cdr lis) (lambda (v1 v2) (return (cons (car lis) v2) v1))))))

(define split
  (lambda (lis)
    (split-cps lis (lambda (v1 v2) (list v1 v2)))))

; multiply takes a list of numbers and gives their product
(define multiply-orig
  (lambda (lis)
    (if (null? lis)
        1
        (* (car lis) (multiply-orig (cdr lis))))))

; continuation passing style version
(define multiply-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ;; ((zero? (car lis)) 0) ; throw away entire CPS; not good b/c we need to stick with continuation
      ((zero? (car lis)) (break 0)) ; 2 call stacks involved now
      (else (multiply-cps (cdr lis) (lambda (v) (return (* (car lis) v))) break)))))

; splitk: takes a list of atoms and returns 2 lists, one with the first k atoms and the other with the rest
; (splitk 4 '(a b c d e f g)) => '((a b c d) (e f g))
(define splitk-cps
  (lambda (k lis return)
    (cond
      ((null? lis) (return '() '()))
      ((zero? k) (return '() lis))
      (else (splitk-cps (- k 1) (cdr lis) (lambda (v1 v2) (return (cons (car lis) v1) v2)))))))


; odd-even: takes a list of numbers and returns 2 lists; 1 with odd numbers, other with even numbers
; (odd-even '(3 5 6 8 9 10 12)) => '((3 5 9) (6 8 10 12))
(define odd-even-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((odd? (car lis)) (odd-even-cps (cdr lis) (lambda (odds evens) (return (cons (car lis) odds) evens))))
      (else (odd-even-cps (cdr lis) (lambda (odds evens) (return odds (cons (car lis) evens))))))))


; multiply*: takes a lsit of numbers that can contain sublists of numbers and it returns the product of
;            all the numbers and also immediately returns on finding a 0 anywhere
