#lang racket

; myappend
(define myappend
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (myappend (cdr lis1) lis2)))))

; (myreverse '(a b c d e)) => '(e d c b a)
(define myreverse
  (lambda (lis)
    (if (null? lis)
        '()
        (myappend (myreverse (cdr lis)) (cons (car lis) '())))))

; (mymap '(1 3 5) (lambda (x) (+ 10 x))) => '(11 13 15)
(define mymap
  (lambda (lis f)
    (cond
      ((null? lis) '())
      (else (cons (f (car lis)) (mymap (cdr lis) f))))))

; removeall* removes all of a specific atom from a list where the list contains sublists
; (removeall* 'x '(a b (((c d) d x) e ((f x) g x x h)))) => (a b (((c d) e ((f) g h)))
; With atom, remove first atom, return the rest of the list
; Now, car is a list. First, recurse on the car, cdr, and cons the two results
(define removeall*
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (removeall* x (car lis)) (removeall* (x (cdr lis)))))
      ((eq? (car lis) x) (removeall* (x (cdr lis))))
      (else (cons (car lis) (removeall* (x (cdr lis))))))))

; member*? is an atom x inside a list where the list can contain sublists
(define member*?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((list? (car lis)) (or (member*? x (car lis)) (member*? x (cdr lis))))
      ((eq? x (car lis)) #t)
      (else (member*? x (cdr lis))))))

; (replaceall* 'x 'y '(a b ((c d x) e x))) => '(a b ((c d y) e y))
(define replaceall*
  (lambda (x y lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (replaceall* x y (car lis)) (replaceall* x y (cdr lis))))
      ; Could use 'pair?' b/c nothing to replace if no cdr
      ((eq? x (car lis)) (cons y (replaceall* x y (cdr lis))))
      (else (cons (car lis) (replaceall* x y (cdr lis)))))))

; (sumnumbers* '(a 1 2 ((e 3) (((((4)))) 5))) => 15
(define sumnumbers*
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((number? (car lis)) (+ (car lis) (sumnumbers* (cdr lis))))
      ((list? (car lis)) (+ (sumnumbers* (car lis)) (sumnumbers* (cdr lis))))
      (else (sumnumbers* (cdr lis))))))

; emptyall - takes a list and removes all atoms, but keeps all parentheses
; (emptyall '(a ((b c) ((d)) () (((e f) g)))))
(define emptyall
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (emptyall (car lis)) (emptyall (cdr lis))))
      (else (emptyall (cdr lis))))))

; flatten - takes a list that may contain lists and returns a list of just the non-empty list atoms of that list
; (flatten '(a ((b c) ((d)) () (((e f) g)))))
(define flatten
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (myappend (flatten (car lis)) (flatten (cdr lis))))
      (else (cons (car lis) (flatten (cdr lis)))))))