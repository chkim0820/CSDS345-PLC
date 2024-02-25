#lang racket

; Normal (not tail recursive) factorial function
(define factorial1
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial1 (- n 1))))))
; more action done once value passed down from recursive calls

; Tail recursive; accumulator passing style
(define factorial-acc
  (lambda (n a)
    (if (zero? n)
        a
        (factorial-acc (- n 1) (* n a)))))
; No action required once recursive calls are complete

(define factorial
  (lambda (n)
    (factorial-acc n 1)))

;; Practices

; numoccurring-acc returns the number of times atom occurs in the list
(define numoccurring-acc
  (lambda (x lis acc)
    (cond
      ((null? lis) acc)
      ((eq? (car lis) x) (numoccurring-acc x (cdr lis) (+ 1 acc)))
      (else (numoccurring-acc x (cdr lis) acc)))))

; sumnumbers-acc returns the sum of all numbers in the list
(define sumnumbers-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((number? (car lis)) (sumnumbers-acc (cdr lis) (+ acc (car lis))))
      (else (sumnumbers-acc (cdr lis) acc)))))