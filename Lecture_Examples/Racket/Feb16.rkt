#lang racket

; remove first
(define removefirst-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (return (cdr lis)))
      (else (removefirst-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; removeall* the list can contain sublists
(define removeall*-orig
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (removeall*-orig x (car lis)) (removeall*-orig x (cdr lis))))
      ((eq? x (car lis)) (removeall*-orig x (cdr lis)))
      (else (cons (car lis) (removeall*-orig x (cdr lis)))))))

(define removeall*-cps
  (lambda (x lis return) 
    (cond
      ((null? lis)       (return '()))
      ((list? (car lis)) (removeall*-cps x (car lis)
                               (lambda (v_car) (removeall*-cps x (cdr lis)
                                       (lambda (v_cdr) (return (cons v_car v_cdr)))))))
      ; ((list? (car lis)) (removeall*-cps x (car lis) (lambda (v_car) (return (cons v_car (removeall*-cps x (cdr lis) (lambda (v) v)))))))
      ; not tail-recursive; will have more stacks
      ((eq? x (car lis)) (removeall*-cps x (cdr lis) return))
      (else              (removeall*-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; replaceall*

; sumnumbers*

; make reverse
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

(define myreverse
  (lambda (lis)
    (if (null? lis)
        '()
        (myappend (myreverse (cdr lis)) (list (car lis))))))

(define myappend-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (myappend-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

(define myreverse-cps
  (lambda (lis return)
    (if (null? lis)
        (return '())
        (myreverse-cps (cdr lis) (lambda (v1) (myappend-cps v1 (list (car lis)) return))))))

; (lambda (v) v) is adding another frame

; flatten
(define myflatten
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (myappend (flatten (car lis)) (flatten (cdr lis))))
      (else (cons (car lis) (flatten (cdr lis)))))))
