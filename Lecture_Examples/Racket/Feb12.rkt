#lang racket

;;(define Minteger
;;  (lambda (lis)
;;    (cond
;;      ((null? lis) '())
;;      ((or (equal? (car (cdr lis)) '*) (equal? (car (cdr lis)) '/)) ((car (cdr lis)) (car lis) (Minteger (cdr lis))))
;;      ((or (equal? (car (cdr lis)) )
;;      )))

;; Empty case not necessarily needed; don't need to do responsibilities of parser/compilers
;; Good inputs expected
;; Every input will also be in a valid form (BNF)

(define M_integer
  (lambda (intexp)
    (cond
      ((number? intexp) intexp)
      ((eq? (car (cdr intexp)) '+) (+ (M_integer (car intexp)) (M_integer (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '-) (- (M_integer (car intexp)) (M_integer (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '*) (* (M_integer (car intexp)) (M_integer (car (cdr (cdr intexp))))))
      ((eq? (car (cdr intexp)) '*) (quotient (M_integer (car intexp)) (M_integer (car (cdr (cdr intexp)))))) 
      ((eq? (car (cdr intexp)) '%) (remainder (M_integer (car intexp)) (M_integer (car (cdr (cdr intexp))))))))) ; diff from mod

;; Making it valid for Scheme form (i.e. (* 2 3))
(define M_integer1
  (lambda (intexp)
    (cond
      ((number? intexp) intexp)
      ((eq? (operator intexp) '+) (+ (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '-) (- (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '*) (* (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp))))
      ((eq? (operator intexp) '*) (quotient (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp)))) 
      ((eq? (operator intexp) '%) (remainder (M_integer (leftoperand intexp)) (M_integer (rightoperand intexp)))))))

;; Defining the names used above
(define operator (lambda (exp) (car exp)))
(define leftoperand cadr) ; cadr: car of cdr
(define rightoperand caddr) ; caddr: car of cdr of cdr


;; basic factorial function
(define factorial-basic
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))


;; Factorial using accumulator
(define factorial-acc
  (lambda (n acc) ; accumulator to collect the result
    (if (zero? n)
        acc
        (factorial-acc (- n 1) (* n acc)))))


;; For function above
(define factorial
  (lambda (n)
    (factorial-acc n 1)))


;; (numoccurring 'x '(a b x c d x e x x g)) => 4
;; Write this in accumulator passing style
(define numoccurring
  (lambda (x lis acc)
    (cond
      ((null? lis) ))
    (if (null? lis)
        acc
        (numoccurring x (cdr lis) (+ 1 acc)))))