; add to the end of the list explicitly
(define add2end-bad
  (lambda (x lis)
    (if (null? lis)
        (cons x '())
        (begin (add2end-bad-helper x lis) lis ))))
        ; will return the lis no matter what

(define add2end-bad-helper
  (lambda (x lis)
    (if (null? (cdr lis))
        (set-cdr! lis (cons x '()))
        (add2end-bad-helper x (cdr lis)))))
; not referentially transparent!

(define fun-bad
  (lambda (lis)
    (append (add2end-bad 'x lis) (add2end-bad 'y lis))))