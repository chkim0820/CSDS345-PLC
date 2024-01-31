;;;; ******************************************************
;;;;   Chaehyeon Kim (cxk445)
;;;;   Scheme assignment for CSDS 345
;;;; ******************************************************

#lang racket

;; helper) appends two lists together
(define myappend
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (myappend (cdr lis1) lis2)))))

;; 1) takes two integers, say n and k, and returns C(n,k)
(define choose
  (lambda (n k)
    (cond
      ((or (equal? k 0) (equal? n k)) 1) ; C(n,0) = C(n,n) = 1
      (else (* (/ n k) (choose (- n 1) (- k 1))))))) ; C(n,k) = n/k * C(n-1,k-1)

;; 2) takes three atoms and a list & returns a list where each occurrence of atom replaced by following
(define rotate
  (lambda (a b c lis)
    (cond
      ((null? lis) '())
      ((equal? a (car lis)) (cons b (rotate a b c (cdr lis)))) ; replace a with b
      ((equal? b (car lis)) (cons c (rotate a b c (cdr lis)))) ; replace b with c
      ((equal? c (car lis)) (cons a (rotate a b c (cdr lis)))) ; replace c with a
      (else (cons (car lis) (rotate a b c (cdr lis))))))) ; not a, b, or c

;; 3) takes atom & list and returns the list with first occurrence of the atom moved to the left
(define moveXleft
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((equal? (car lis) x) (cdr lis)) ; current atom is x => remove
      ((and (pair? (cdr lis)) (equal? (car (cdr lis)) x)) ; if next equals x,
       (cons x (cons (car lis) (cdr (cdr lis))))) ; replace current atom with x & move it after
      (else (cons (car lis) (moveXleft x (cdr lis))))))) ; no x

;; 4) takes function & list; applies function to list and returns containing only true elements
(define myfilter
  (lambda (func lis)
    (cond
      ((null? lis) '())
      ((func (car lis)) (cons (car lis) (myfilter func (cdr lis)))) ; function returns true
      (else (myfilter func (cdr lis)))))) ; else, function returns false
   
;; 5) takes two numbers; compute the squareroot of the value using iteration rounds of Newton's method
(define squareroot
  (lambda (val it) ; value and iteration
    (cond
      ((zero? it) val) ; zero iteration left
      (else (- (squareroot val (- it 1))
               (/ (- (* (squareroot val (- it 1)) (squareroot val (- it 1))) val)
                  (* 2 (squareroot val (- it 1))))))))) ; new = old - ((old * old) - value) / (2 * old)

;; 6) takes three atoms and a list; returns same list but each occurrence replaced by following atom
(define rotate*
  (lambda (a b c lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (rotate* a b c (car lis)) (rotate* a b c (cdr lis)))) ; currently a list
      ((equal? (car lis) a) (cons b (rotate* a b c (cdr lis)))) ; currently a => replace with b
      ((equal? (car lis) b) (cons c (rotate* a b c (cdr lis)))) ; currently b => replace with c
      ((equal? (car lis) c) (cons a (rotate* a b c (cdr lis)))) ; currently c => replace with a
      (else (cons (car lis) (rotate* a b c (cdr lis))))))) ; not a, b, or c

;; 7) takes number & list; all parentheses removed from sublist nested more than N deep
(define flattenN
  (lambda (N lis)
    (cond
      ((null? lis) '())
      ((and (list? (car lis)) (equal? N 1)) ; is currently a list & lowest depth reached
       (myappend (flattenN N (car lis)) (flattenN N (cdr lis)))) ; merge w/o parenthese
      ((list? (car lis)) ; list but depth limit not reached
       (cons (flattenN (- N 1) (car lis)) (flattenN N (cdr lis))))
      (else (cons (car lis) (flattenN N (cdr lis))))))) ; not a list

;; 8) takes two vectors; returns outerproduct of vectors
(define outerproduct
  (lambda (vec1 vec2)
    (cond
      ((or (null? vec1) (null? vec2)) '())
      ((and (pair? (cdr vec2)) (not (pair? (cdr (cdr vec2))))) ; cdr is last element of lis2
       (cons (outerproduct vec1 (cons (car vec2) '())) (cons (outerproduct vec1 (cdr vec2)) '())))
      ((pair? (cdr vec2)) ; cdr of vec2 is not the last in vec2; don't add the extra ()
       (cons (outerproduct vec1 (cons (car vec2) '())) (outerproduct vec1 (cdr vec2))))
      (else ; car of vec2 is the only element; do the multiplication
       (cons (* (car vec1) (car vec2)) (outerproduct (cdr vec1) vec2))))))

;; 9) takes a list; returns the largest number in the list
(define maxvalue*
  (lambda (lis)
    (cond
      ((null? lis) 'novalue)
      ((list? (car lis)) ; if currently a list; go through its sublists
       (maxvalue* (cons (maxvalue* (car lis)) (cdr lis)))) ; add sublists' biggest to cdr
      ((not (number? (car lis))) (maxvalue* (cdr lis))) ; currently not a number; ignore
      ((not (number? (maxvalue* (cdr lis)))) (car lis)) ; (car lis) is a number
      ((> (car lis) (maxvalue* (cdr lis))) (car lis)) ; compare if two numbers; car bigger
      (else (maxvalue* (cdr lis)))))) ; recursed value bigger

;; helper) helper function for moveXleft*; returns true if the next atom equals x, the input atom
(define search
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((list? (car lis)) (or (search x (car lis)) (search x (cdr lis)))) ; car is list
      ((equal? (car lis) x) #t) ; car equals x
      (else (search x (cdr lis)))))) ; car does not equal x

;; 10) takes an atom and a list with sublists; returns the same except
;;     the first occurrence of the atom X in the list shifted to left
(define moveXleft*
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((equal? (car lis) x) (cdr lis)) ; car is x; get rid of it
      ((and (pair? (car lis)) (search x (car lis))) ; In car's sublist, there's an x
       (cons (moveXleft* x (car lis)) (cdr lis)))
      ((and (pair? (car lis)) (equal? x (car (cdr lis)))) ; currently a list & bring x in
       (cons (myappend (car lis) (cons x '())) (moveXleft* x (cdr lis)))) ; add x at the end of current list
      ((and (not (list? (car lis))) (equal? x (cdr (car lis)))) ; car/cdr not list & cdr==x
       (cons x (cons (car lis) (moveXleft* x (cdr lis))))) ; swap x and car
      ((and (pair? (cdr lis)) (equal? x (car (car (cdr lis))))) ; cdr is a list containing x first
       (cons (car lis) (cons x (moveXleft* x (cdr lis))))) ; add x after car
      (else (cons (car lis) (moveXleft* x (cdr lis))))))) ; Else, just keep recursing


;;;; **********************************************************************
;;;; Outline) broadly 3 categories for moveXleft*
;;;; Still return '() when lis null
;;;; When x encountered, get rid of it
;;;; Use a helper function to see if a car sublist contains x
;;;;     => only first occurrence removed; recurse on that & append cdr
;;;; 1) out => in (currently a list & bring x in); create a new list & append? (cdr is x & car is list)
;;;; 2) stays in/out; cdr is x & car is not a list; no list involved => do similar to moveXleft
;;;; 3) in => out; have to look into list; add to after car (myappend)
;;;;    Special cases: (((x)) vs. (x)); 1st sublist with x in front
;;;;        Look exactly if a sublist starts with x; just another if statement
;;;;            if car is a list & its first element is a sublist with x in the front,
;;;;            make the first element x & add the sublist next
;;;; If nothing, just keep recursing
;;;; ***********************************************************************




;(define moveXleft* ; works for all X, not just first
;  (lambda (x lis) 
;    (cond
;      ((null? lis) '())
;      ((equal? (car lis) x) (cdr lis))
;      ;; List containing x
;      ((and (list? (car lis)) (search x (car lis))) (cons (moveXleft* x (car lis)) (cdr lis)))
;      ;; Current element is list & next is x
;      ((and (list? (car lis)) (and (pair? (cdr lis)) (equal? x (car (cdr lis)))))
;       (cons (myappend (moveXleft* x (car lis)) (cons x '())) (moveXleft* x (cdr lis))))
;      ;; Current atom & next is x
;      ((and (pair? (cdr lis)) (equal? (car (cdr lis)) x)) (cons x (cons (car lis) (cdr (cdr lis)))))
;      ;; x first in the list
;      ((and (and (pair? (cdr lis)) (pair? (car (cdr lis)))) (equal? (car (car (cdr lis))) x))
;       (myappend (cons (car lis) (cons x '())) (moveXleft* x (cdr lis)))) 
;      (else (cons (car lis) (moveXleft* x (cdr lis)))))))