#lang racket

; split: (split '(a b c d e)) ==> ((a c e) (b d))
(define split
  (lambda (lis)
    (cond
      ((null? lis) '(() ()))
      (else (cons (cons (car lis) (car (cdr (split (cdr lis))))) (cons (car (split (cdr lis))) '()))))))

; split in CPS
(define split-cps
  (lambda (lis return)
    (if (null? lis)
        (return '() '())
        (split-cps (cdr lis) (lambda (v1 v2) (return v2 (cons (car lis) v1)))))))

; multiply a list of numbers
(define multiply-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ((zero? (car lis)) (break 0)) ; keep it as (lambda (v) v) and return 0 immediately
      (else (multiply-cps (cdr lis) (lambda (v) (return (* (car lis) v))) break)))))

;; PRACTICES

; splitk-cps
(define splitk-cps
  (lambda (k lis return)
    (cond
      ((null? lis) (return '() '()))
      ((eq? k 0) (return '() lis))
      (else (splitk-cps (- k 1) (cdr lis) (lambda (v1 v2) (return (cons (car lis) v1) v2)))))))

(define splitk
  (lambda (k lis)
    (splitk-cps k lis (lambda (v1 v2) (list v1 v2)))))


; odd-even-cps returns a list with two sublists; odd and even
(define odd-even-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((odd? (car lis)) (odd-even-cps (cdr lis) (lambda (v1 v2) (return (cons (car lis) v1) v2))))
      ((even? (car lis)) (odd-even-cps (cdr lis) (lambda (v1 v2) (return v1 (cons (car lis) v2)))))
      (else (odd-even-cps (cdr lis) return)))))

(define odd-even
  (lambda (lis)
    (odd-even-cps lis (lambda (v1 v2) (list v1 v2)))))


; multiply*-cps returns the product of all numbers & immediately returns 0 if encountered
(define multiply*-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ((list? (car lis)) (multiply*-cps (car lis) (lambda (v1)
                                                    (multiply*-cps (cdr lis)
                                                                   (lambda (v2) (return (* v1 v2))) break)) break))
      ((zero? (car lis)) (break 0))
      ((number? (car lis)) (multiply*-cps (cdr lis) (lambda (v) (return (* (car lis) v))) break))
      (else (multiply*-cps (cdr lis) return break)))))

(define multiply*
  (lambda (lis)
    (multiply*-cps lis (lambda (v1) v1) (lambda (v2) v2))))