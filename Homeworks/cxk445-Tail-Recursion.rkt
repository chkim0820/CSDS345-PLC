#lang racket

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
                                               (replacefirstk*-cps new-k (cdr lis1) new-lis2
                                                                   (lambda (v) (return (cons prev v))) ; new return func 
                                                                   (lambda (v1 v2 v3) v3)))))
      (else (replacefirstk*-cps (- k 1) (cdr lis1) (cdr lis2) (lambda (v) (return (cons (car lis2) v))) next)))))

(define replacefirstk*
  (lambda (k lis1 lis2)
    (replacefirstk*-cps k lis1 lis2 (lambda (v) v) (lambda (v1 v2 v3) v3)))) ; next stores function for the cdr and after sublists

;; 8) Using CPS, moveAllXleft* moves every atom x one space to the left
(define moveAllXleft*-cps
  (lambda (x lis return)
    (cond
      ((or (null? lis) (not (list? lis))) (return lis)) ; lis not a list or null list
      ((eq? x (car lis)) (moveAllXleft*-cps x (cdr lis) return)) ; current is x; not appending the current x
      ((not (pair? (cdr lis))) (moveAllXleft*-cps x (car lis) ; cdr is null list
                                                  (lambda (v) (return (cons v '())))))
      ;; next is x
      ((and (list? (car lis)) (eq? x (cadr lis))) ; next is x & car is a list; ex) () x => (x)
       (moveAllXleft*-cps x (car lis) (lambda (v1) (moveAllXleft*-cps x (cdr lis) (lambda (v2) (return (cons (myappend v1 (cons x '())) v2))))))) ; FIX
      ((eq? x (cadr lis)) (moveAllXleft*-cps x (cdr lis) (lambda (v) (return (list x (car lis) v))))) ; next is x & car is not a list 

      ;; next is a list with x in front; car can be a list or not a list
      ((and (pair? (cadr lis)) (eq? x (car (cadr lis))))
       (moveAllXleft*-cps x (car lis) (lambda (v1) (moveAllXleft*-cps x (cdr lis)
                                                                      (lambda (v2) (return (list v1 x v2))))))) ; next is a list with x in front
      
      ((pair? (car lis)) (moveAllXleft*-cps x (car lis) ; car is list & next is not (no x)
                                            (lambda (v1) (moveAllXleft*-cps x (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
      (else (moveAllXleft*-cps x (cdr lis) (lambda (v) (return (cons (car lis) v)))))))) ; none of the above

(define moveAllXleft*
  (lambda (x lis)
    (moveAllXleft*-cps x lis (lambda (v) v))))

; helper) myappend appends two lists together
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1 (myappend (cdr l1) l2))))))
; cases:
; 1) current is x
; 2) next is x & curr list
; 3) next not x & curr list
; 4) next is x & curr not list
; 5) next is a list with x in front (curr not list)
; FIX?: ()(X) => ()X()

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