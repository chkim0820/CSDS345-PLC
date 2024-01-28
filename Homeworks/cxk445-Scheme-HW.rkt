; Chaehyeon Kim (cxk445); Scheme assignment for CSDS 345
#lang racket

; myappend; helper function
(define myappend
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (myappend (cdr lis1) lis2)))))

; 1) takes two integers, say n and k, and returns C(n,k)
(define choose
  (lambda (n k)
    (cond
      ((or (equal? k 0) (equal? n k)) 1)
      (else (* (/ n k) (choose (- n 1) (- k 1)))))))

; 2) takes three atoms and a list & returns a list where each occurrence of atom replaced by following
(define rotate
  (lambda (a b c lis)
    (cond
      ((null? lis) '())
      ((equal? a (car lis)) (cons b (rotate a b c (cdr lis))))
      ((equal? b (car lis)) (cons c (rotate a b c (cdr lis))))
      ((equal? c (car lis)) (cons a (rotate a b c (cdr lis))))
      (else (cons (car lis) (rotate a b c (cdr lis)))))))

; 3) takes atom & list and returns the list with first occurrence of the atom moved to the left
(define moveXleft
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((equal? (car lis) x) (cdr lis))
      ((and (not (null? (cdr lis))) (equal? (car (cdr lis)) x)) (cons x (cons (car lis) (cdr (cdr lis)))))
      (else (cons (car lis) (moveXleft x (cdr lis)))))))

; 4) takes function & list; applies function to list and returns containing only true elements
(define myfilter
  (lambda (func lis)
    (cond
      ((null? lis) '())
      ((func (car lis)) (cons (car lis) (myfilter func (cdr lis))))
      (else (myfilter func (cdr lis))))))
   
; 5) takes two numbers; compute the squareroot of the value using iteration rounds of Newton's method
(define squareroot
  (lambda (val it)
    (cond
      ((zero? it) val)
      (else (- (squareroot val (- it 1)) (/ (- (* (squareroot val (- it 1)) (squareroot val (- it 1))) val) (* 2 (squareroot val (- it 1)))))))))

; 6) takes three atoms and a list; returns a list that is the same as the input list but each occurrence replaced by following atom
(define rotate*
  (lambda (a b c lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (rotate* a b c (car lis)) (rotate* a b c (cdr lis))))
      ((equal? (car lis) a) (cons b (rotate* a b c (cdr lis))))
      ((equal? (car lis) b) (cons c (rotate* a b c (cdr lis))))
      ((equal? (car lis) c) (cons a (rotate* a b c (cdr lis))))
      (else (cons (car lis) (rotate* a b c (cdr lis)))))))

; 7) takes number & list; all parentheses removed from sublist nested more than N deep
(define flattenN
  (lambda (N lis)
    (cond
      ((null? lis) '())
      ((and (list? (car lis)) (equal? N 1)) (myappend (flattenN 1 (car lis)) (flattenN N (cdr lis))))
      ((list? (car lis)) (cons (flattenN (- N 1) (car lis)) (flattenN N (cdr lis))))
      (else (cons (car lis) (flattenN N (cdr lis)))))))
      ; can i do this without helper function?

; 8) takes two vectors; returns outerproduct of vectors
(define outerproduct
  (lambda (vec1 vec2)
    (cond
      ((or (null? vec1) (null? vec2)) '())
      ((and (pair? (cdr vec2)) (not (pair? (cdr (cdr vec2))))) (cons (outerproduct vec1 (cons (car vec2) '())) (cons (outerproduct vec1 (cdr vec2)) '())))
      ((pair? (cdr vec2)) (cons (outerproduct vec1 (cons (car vec2) '())) (outerproduct vec1 (cdr vec2))))
      (else (cons (* (car vec1) (car vec2)) (outerproduct (cdr vec1) vec2))))))

; 9) takes a list; returns the largest number in the list
(define maxvalue*
  (lambda (lis)
    (cond
      ((null? lis) 'novalue)
      ((list? (car lis)) (maxvalue* (cons (maxvalue* (car lis)) (cdr lis)))) ; fix
      ((not (number? (car lis))) (maxvalue* (cdr lis)))
      ((not (number? (maxvalue* (cdr lis)))) (car lis)) ; (car lis) is a number
      ((> (car lis) (maxvalue* (cdr lis))) (car lis))
      (else (maxvalue* (cdr lis))))))
      ; more efficient version?

; 10) takes an atom and a list with sublists; returns the same except the first occurrence of the atom X in the list shifted to left
(define moveXleft*
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((equal? (car lis) x) (cdr lis)) ; fix?
      ((and (and (pair? (cdr lis)) (pair? (car (cdr lis)))) (equal? (car (car (cdr lis))) x)) (cons x (cons (moveXleft* x (car lis)) (moveXleft* x (cdr lis)))))
      ((and (pair? (cdr lis)) (equal? (car (cdr lis)) x)) (cons x (moveXleft* x (cdr lis))))
      ((list? (car lis)) (cons (moveXleft* x (car lis)) (moveXleft* x (cdr lis))))
      (else (cons (car lis) (moveXleft* x (cdr lis)))))))