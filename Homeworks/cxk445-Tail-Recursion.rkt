#lang racket
;;; By Chaehyeon Kim (cxk445) for CSDS 345 Continuation and Tail Recursion Programming Exercise

;; 1) Using accumulator passing style, choose returns the combinatorial choose function
(define choose-aps
  (lambda (n k acc)
    (cond
      ((or (eq? n k) (zero? k)) (* acc 1))
      (else (choose-aps (- n 1) (- k 1) (* acc (/ n k)))))))

(define choose
  (lambda (n k)
    (choose-aps n k 1)))

;; 2) Using CPS, rotate returns a list with rotated values
(define rotate-cps
  (lambda (a b c lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? (car lis) a) (rotate-cps a b c (cdr lis) (lambda (v) (return (cons b v)))))
      ((eq? (car lis) b) (rotate-cps a b c (cdr lis) (lambda (v) (return (cons c v)))))
      ((eq? (car lis) c) (rotate-cps a b c (cdr lis) (lambda (v) (return (cons a v)))))
      (else (rotate-cps a b c (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define rotate
  (lambda (a b c lis)
    (rotate-cps a b c lis (lambda (v) v))))

;; 3) Using CPS, moveXleft returns the first occurence of x moved one space to left
(define moveXleft-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '())) ; at the end of the list
      ((eq? x (car lis)) (return (cdr lis))) ; not adding current x
      ((and (pair? (cdr lis)) (eq? x (cadr lis))) (return (cons x (cons (car lis) (cddr lis))))) ; next is x
      (else (moveXleft-cps x (cdr lis) (lambda (v) (return (cons (car lis) v)))))))) 

(define moveXleft
  (lambda (x lis)
    (moveXleft-cps x lis (lambda (v) v))))

;; 4) Using CPS, squareroot returns the squareroot (Newton's method: new = old - (old * old - value) / (2 * old))
(define squareroot-cps
  (lambda (val it return)
    (cond
      ((zero? it) (return val))
      (else (squareroot-cps val (- it 1)
                            (lambda (v) (return (- v (/ (- (* v v) val) (* 2 v))))))))))

(define squareroot
  (lambda (val it)
    (squareroot-cps val it (lambda (v) v))))

;; 5) Using CPS, rotate* returns a list containing sublists with the three atoms rotated
(define rotate*-cps
  (lambda (x y z lis return)
    (cond
      ((null? lis) (return '()))
      ((pair? (car lis)) (rotate*-cps x y z (car lis) ; current atom is a non-empty list
                                      (lambda (v1) (rotate*-cps x y z (cdr lis)
                                                                (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car lis) x) (rotate*-cps x y z (cdr lis) (lambda (v) (return (cons y v))))) ; current = x
      ((eq? (car lis) y) (rotate*-cps x y z (cdr lis) (lambda (v) (return (cons z v))))) ; current = y
      ((eq? (car lis) z) (rotate*-cps x y z (cdr lis) (lambda (v) (return (cons x v))))) ; current = z
      (else (rotate*-cps x y z (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define rotate*
  (lambda (x y z lis)
    (rotate*-cps x y z lis (lambda (v) v))))

;; 6) Using CPS, maxvalue* returns the largest number in the list with sublists
(define maxvalue*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 'novalue))
      ((list? (car lis)) (maxvalue*-cps (car lis) (lambda (v1) (maxvalue*-cps (cdr lis) (lambda (v2) (return (max v1 v2)))))))
      (else (maxvalue*-cps (cdr lis) (lambda (v) (return (max (car lis) v))))))))

(define maxvalue*
  (lambda (lis)
    (maxvalue*-cps lis (lambda (v) v))))

; Helper function that returns the bigger of the two input numbers
(define max
  (lambda (x y)
    (cond
      ((eq? y 'novalue) x)
      ((eq? x 'novalue) y)
      ((> x y) x)
      (else y))))

;; 7) Using CPS, replacefirstk* replaces the first k atoms in the first list with first k atoms in second list
;; lis1 is the list that may contain sublists & lis2 is the list of atoms to replace with
(define replacefirstk*-cps
  (lambda (k lis1 lis2 return next)
    (cond
      ((or (zero? k) (or (null? lis1) (null? lis2))) (next k lis2 (return lis1)))
      ((pair? (car lis1)) (replacefirstk*-cps k (car lis1) lis2 (lambda (v) v) ; parenthesis?
                                              (lambda (new-k new-lis2 prev) ; this is the next function for cdr
                                                (replacefirstk*-cps new-k (cdr lis1) new-lis2 ; has new functions
                                                                    (lambda (v) (return (cons prev v))) next))))
      (else (replacefirstk*-cps (- k 1) (cdr lis1) (cdr lis2) (lambda (v) (return (cons (car lis2) v))) next)))))

(define replacefirstk*
  (lambda (k lis1 lis2)
    (replacefirstk*-cps k lis1 lis2 (lambda (v) v) (lambda (v1 v2 v3) v3)))) ; next stores function for the cdr and after sublists

;; 8) Using CPS, moveAllXleft* moves every atom x one space to the left
(define moveAllXleft*-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '())) ; null list
      ((and (pair? (car lis)) (eq? x (car (car lis)))) ; Special case: ((x))
       (moveAllXleft*-cps x (car lis) (lambda (v1)
                                        (moveAllXleft*-cps x (cdr lis) (lambda (v2) (return (cons x (cons v1 v2))))))))
      ((and (null? (cdr lis)) (pair? (car lis))) ; cdr is null (nothing after) & currently list
       (moveAllXleft*-cps x (car lis) (lambda (v) (return (list v)))))
      ((and (null? (cdr lis)) (eq? x (car lis))) (return '()))
      ((null? (cdr lis)) (return lis)) ; cdr is null (nothing after) & currently list
      ((and (eq? x (car lis)) (eq? x (cadr lis))) (moveAllXleft*-cps x (cdr lis) (lambda (v) (return (cons x v)))))
      ((eq? x (car lis)) (moveAllXleft*-cps x (cdr lis) return)) ; car is x 
      
      ((and (eq? x (cadr lis)) (list? (car lis))) ; car is list & next is x
       (moveAllXleft*-cps x (car lis) (lambda (v1) (moveAllXleft*-cps x (cdr lis)
                                                                      (lambda (v2) (return (cons (myappend v1 (list x)) v2)))))))
      ((eq? x (cadr lis)) (moveAllXleft*-cps x (cdr lis) (lambda (v) (return (cons x (cons (car lis) v)))))) ; next is x & curr not list
      ((pair? (car lis)) (moveAllXleft*-cps x (car lis) ; next is not x & curr list
                                            (lambda (v1) (moveAllXleft*-cps x (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
      (else (moveAllXleft*-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))
 
(define moveAllXleft*
  (lambda (x lis)
    (moveAllXleft*-cps x lis (lambda (v) v))))

; helper) myappend appends two lists together
(define myappend 
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))
; cases:
; 1) Null list
; 2) current is x
; special: ((x))
; 3) nothing after & car is a list
; 4) nothing after & car is not a list

; 5) next is x & curr list
; 6) next is x & curr not list
; 7) next is not x & curr list
; 8) next is a list with x in front (curr not list)

;; 9) Using call/cc, collapse-x returns with all atoms between the given atoms collapsed
(define collapse-x-cc
  (lambda (x lis break)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (break (collapse-x-cc x (cdr lis) break)))
      (else (cons (car lis) (collapse-x-cc x (cdr lis) break))))))

(define collapse-x
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (cons (car lis) (call/cc (lambda (break) (collapse-x-cc x (cdr lis) break)))))
      (else (cons (car lis) (collapse-x x (cdr lis)))))))


;; 10) Using call/cc, xindex empties any sublists containing the given atom & replace w/ index
(define xindex-cc
  (lambda (x lis ind break)
    (cond
      ((null? lis) '())
      ((eq? (car lis) x) (break (list ind)))
      ((pair? (car lis)) (cons (xindex x (car lis)) (xindex-cc x (cdr lis) (+ 1 ind) break)))
      (else (cons (car lis) (xindex-cc x (cdr lis) (+ 1 ind) break))))))

(define xindex
  (lambda (x lis)
    (call/cc (lambda (break) (xindex-cc x lis 1 break)))))