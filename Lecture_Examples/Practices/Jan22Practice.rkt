#lang racket

;; Takes a list of atoms and returns a list that is the reverse of the input
(define myreverse
  (lambda (lis)
    (cond
      ((null? (cdr lis)) (car lis))
      (else (cons (myreverse (cdr lis)) (cons (car lis) '() ))))))

;; Takes a list and returns a list that is the result of applying the function to each element


;;;; NOT DONE

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (insertR (new old (cdr lat)))))
      (else (cons (car lat) (insertR new old (cdr lat))))))))