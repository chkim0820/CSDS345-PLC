; Run this code in R5S5 scheme instead of racket because of the use of set-cdr!

; add an atom to the end of a list
(define add2end
  (lambda (x lis)
    (if (null? lis)
        (cons x '())
        (cons (car lis) (add2end x (cdr lis))))))

(define fun
  (lambda (lis)
    (append (add2end 'x lis) (add2end 'y lis))))

; add to the end of the list explicitly
(define add2end-bad
  (lambda (x lis)
    (if (null? lis)
        (cons x '())
        (begin (add2end-bad-helper x lis) lis))))

(define add2end-bad-helper
  (lambda (x lis)
    (if (null? (cdr lis))
        (set-cdr! lis (cons x '()))
        (add2end-bad-helper x (cdr lis)))))

(define fun-bad
  (lambda (lis)
    (append (add2end-bad 'x lis) (add2end-bad 'y lis))))