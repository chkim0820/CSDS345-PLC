#lang racket
; count the number of times an atom occurs in a list
; use tail recursion and accumulator passing style; reuses frames
(define numoccuring-acc
  (lambda (x lis acc)
    (cond
      ((null? lis) acc)
      ((eq? x (car lis)) (numoccuring-acc x (cdr lis) (+ 1 acc)))
      (else (numoccuring-acc x (cdr lis) acc)))))

(define numoccuring
  (lambda (x lis)
    (numoccuring-acc x lis 0)))

; (sumnumbers '(a b 1 2 c 3 4 d)) => 10
(define sumnumbers-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((number? (car lis)) (sumnumbers-acc (cdr lis) (+ (car lis) acc)))
      (else (sumnumbers-acc (cdr lis) acc)))))

(define sumnumbers
  (lambda (lis)
    (sumnumbers-acc lis 0)))

; squareroot
(define squareroot-orig
  (lambda (n k)
    (if (zero? k)
        n
        ; new = old - ((old * old) - value) / (2 * old))
        ; will be exponential but ok because not that many calls
        (- (squareroot-orig n (- k 1)) (/ (- * (squareroot-orig n (- k 1)) (squareroot-orig n (- k 1))) n ) (* 2 (squareroot-orig n (- k 1)))))))

; squareroot with tail recursion
(define squareroot-acc
  (lambda (n k old)
    (if (zero? k)
        old
        (squareroot-acc n (- k 1) (- old (/ (- (old * old) n) (* 2 old))))))) ; old kind of like variable reassignment in a way but not really
                                                                              ; since recursion used; different logic

;===================================================
; CONTINUATION PASSING STYLE
;===================================================

(define factorial-orig
  (lambda (n)
    (if (zero? n)
        1
        (* (factorial-orig (- n 1))))))

; write factorial using a continuation function for the return value
; and make it tail recursive
; less efficient than acc. passing style
(define factorial-cps ; list return value explicitly; similar to imperative
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* v n))))))) ; lambda (v) is the value returned by the recursive call

(define factorial
  (lambda (n)
    (factorial-cps n (lambda (v) v))))

; remove the first occurrence of an atom in a list of atoms
(define remove-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (return (cdr lis)))
      (else (remove-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; removeall of an atom from a list of atoms
; (removeall-cps 'x '(a x b x c) (lambda (v) v)) => '(a b c)
(define removeall-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (removeall-cps x (cdr lis) return))
      (else (removeall-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; replaceall replacing the first atom with the second in a list
; (replaceall-cps 'x 'y '(a x b x c) (lambda (v) v)) => '(a y b y c)
