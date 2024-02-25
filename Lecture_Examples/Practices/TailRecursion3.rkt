#lang racket

; removeall*: remove all occurrences of a variable in a list that can contain sublists
(define removeall*
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (removeall* x (cdr lis)))
      ((list? (car lis)) (cons (removeall* x (car lis)) (removeall* x (cdr lis))))
      (else (cons (car lis) (removeall* x (cdr lis)))))))

; the continuation passing style version that is tail recursive
(define removeall*-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (removeall*-cps x (cdr lis) return)) ; previous function passed on since current element being removed
      ((list? (car lis)) (removeall*-cps x (car lis)
                                         (lambda (v1) (removeall*-cps x (cdr lis)
                                                                      (lambda (v2) (return (cons v1 v2)))))))
      (else (removeall*-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))


;; Continuation Exercise 2

; replaceall*-cps
(define replaceall*-cps
  (lambda (x y lis return)
    (cond
      ((null? lis) (return '()))
      ((equal? x (car lis)) (replaceall*-cps x y (cdr lis) (lambda (v) (return (cons y v)))))
      ((list? (car lis)) (replaceall*-cps x y (car lis) (lambda (v1) (replaceall*-cps x y (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
      (else (replaceall*-cps x y (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; sumnumbers*-cps
(define sumnumbers*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 0))
      ((number? (car lis)) (sumnumbers*-cps (cdr lis) (lambda (v) (return (+ (car lis) v)))))
      ((list? (car lis)) (sumnumbers*-cps (car lis) (lambda (v1) (sumnumbers*-cps (cdr lis) (lambda (v2) (return (+ v1 v2)))))))
      (else (sumnumbers*-cps (cdr lis) return)))))

;; Combining Continuations

; normal append
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

; normal reverse
(define myreverse
  (lambda (lis)
    (if (null? lis)
        '()
        (myappend (myreverse (cdr lis)) (list (car lis))))))

; cps version of append
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

; cps version of reverse
(define reverse-cps
  (lambda (lis return)
    (if (null? lis)
        (return '())
        (reverse-cps (cdr lis) (lambda (v1) (append-cps v1 (list (car lis)) return)))))) ; return: '(lambda (v) v); returning the final value

; practice: flatten-cps returns a list with no sublists
(define flatten-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return lis))
      ((pair? (car lis)) (flatten-cps (car lis) (lambda (v1) (flatten-cps (cdr lis) (lambda (v2) (return (append-cps v1 v2 (lambda (v) v))))))))
      (else (flatten-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))