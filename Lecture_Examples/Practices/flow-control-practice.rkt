#lang racket

;; Using tail recursion and continuation
;; cut: Any values between cut and the next end are not counted in the sum.  However if a cut appears without a later end, the cut is ignored.
;; end: Used to end a subsequence of numbers that are ignored in the sum.  An end that appears without a preceding cut is ignored.
;; allow: If used between cut and end, the number immediately after allow is included in the sum.  allow is ignored if used outside of cut and end.

(define sumwithcut-cps
  (lambda (lis switch return cut)
    (cond
      ((null? lis) (return 0))
      ((eq? 'cut (car lis)) (sumwithcut-cps (cdr lis) 1 return return)) ; reversing orders
      ((and (eq? 'end (car lis)) (eq? switch 1)) (sumwithcut-cps (cdr lis) 0 cut cut))
      ((eq? 'end (car lis)) (sumwithcut-cps (cdr lis) switch return cut))
      ((eq? 'allow (car lis)) (sumwithcut-cps (cdr lis) switch return (lambda (v) (cut (+ (cadr lis) v)))))
      (else (sumwithcut-cps (cdr lis) switch (lambda (v) (return (+ (car lis) v))) cut)))))

(define sum-with-cut
  (lambda (lis)
    (sumwithcut-cps lis 0 (lambda (v) v) (lambda (v) v))))

;; Using call/cc and normal recursion
(define sumwithcut-call
  (lambda (lis cut switch)
    (cond
      ((null? lis) 0)
      ((eq? 'cut (car lis)) (call/cc (lambda (end) (sumwithcut-call (cdr lis) end 1))))
      ((and (eq? switch 1) (eq? 'end (car lis))) (cut (sumwithcut-call (cdr lis) cut 0)))
      ((eq? 'end (car lis)) (sumwithcut-call (cdr lis) cut switch))
      ((and (eq? 'allow (car lis)) (eq? 1 switch)) (cut (+ (cadr lis) (call/cc (lambda (v) (sumwithcut-call (cdr lis) v switch))))))
      ((eq? 'allow (car lis)) (sumwithcut-call (cdr lis) cut switch))
      (else (+ (car lis) (sumwithcut-call (cdr lis) cut switch))))))

(define sumwithcut
  (lambda (lis)
    (call/cc (lambda (cut) (sumwithcut-call lis cut 0)))))