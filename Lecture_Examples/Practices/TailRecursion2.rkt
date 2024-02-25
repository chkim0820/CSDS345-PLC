#lang racket

; normal factorial
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; continuation passing style version of factorial
(define factorial-cps
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* n v)))))))
; the lambda function is the input that is passed on; build on the function
; v is the value returned by the recursive call
; default method call would be: (factorial-cps n (lambda (v) v))


; normal remove function (remove the first occurrence of an atom in a list of atoms)
(define remove
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (cdr lis))
      (else (cons (car lis) (remove x (cdr lis)))))))

; cps version of remove
(define remove-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (return (cdr lis)))
      (else (remove-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))


;; More practices:

; removeall-cps returns a list with all occurrences removed
(define removeall-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return lis))
      ((eq? (car lis) x) (removeall-cps x (cdr lis) (lambda (v) (return v))))
      (else (removeall-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))


; replaceall-cps replaces all copies of the first atom replaced by the second
(define replaceall-cps
  (lambda (x y lis return)
    (cond
      ((null? lis) (return lis))
      ((equal? x (car lis)) (replaceall-cps x y (cdr lis) (lambda (v) (return (cons y v)))))
      (else (replaceall-cps x y (cdr lis) (lambda (v) (return (cons (car lis) v))))))))