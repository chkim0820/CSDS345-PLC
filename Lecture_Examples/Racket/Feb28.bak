#lang racket

; (sum-with-cut '(1 2 3 4)) => 10
; (sum-with-cut '(1 2 cut 3 4 end 5)) => 8
; Implemented using call/cc
(define sum-with-cut
  (lambda (lis cut)
    (cond
      ((null? lis) 0)
      ((eq? (car lis) 'cut) (call/cc (lambda (end) (sum-with-cut (cdr lis) end))))
      ((eq? (car lis) 'allow)   ) ; allow just one element after allow
      ((eq? (car lis) 'end) (cut (sum-with-cut (cdr lis) cut))) ; or could be (lambda (v) v) instead of cut
      (else (+ (car lis) (sum-with-cut (cdr lis) cut))))))

; Implemented using CPS
(define sum-with-cut2
  (lambda (lis return cut)
    (cond
      ((null? lis) (return 0))
      ((eq? (car lis) 'cut) (sum-with-cut2 (cdr lis) return return))
      ((eq? (car lis) 'allow) (sum-with-cut (cdr lis) (lambda (v) (+ (cadr lis) v)) (lambda (v) (cut (+ (cadr lis) v))))) 
      ((eq? (car lis) 'end) (sum-with-cut2 (cdr lis) cut cut))
      (else (sum-with-cut2 (cdr lis) (lambda (v) (return (+ (car lis) v))) cut)))))