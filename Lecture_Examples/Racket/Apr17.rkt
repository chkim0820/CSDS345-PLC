#lang racket

; Let's create the syntax "debug"
; So if I have code like:
;    (+ 1 (debug x))
; this will print the contents of x while still using x in the addition
;(let ((y x)
;      (begin (print y) y)))
(define-syntax debug ; is a macro
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((y ,(cadr slist))) (begin (print y) (newline) y))))) ; giving the context; ` is a list that can be unquoted using ","
; begin takes a sequence of things & returns the last value

; a "for each do" syntax
; (foreachdo x '(1 2 3 4) (* x 2))
; translate this to
; (map (lambda (x) (* x 2)) '(1 2 3 4)
(define-syntax foreachdo
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(map (lambda (,(cadr slist) ) ,(cadddr slist) ) ,(caddr slist) ))))

; Define ++ that increases the value of a "variable" by 1 & return the value before the increment
; (++ x)
; (let ((save x)) (begin (set! x (+ 1 x)) save))
(define-syntax ++
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(let ((save ,(cadr slist))) (begin (set! ,(cadr slist) (+ 1 save)) save)))))
    ; (datum->syntax syn `(begin (print ,(cadr slist)) (set! ,(cadr slist) (+ 1 ,(cadr slist)))))))


; Define println:
(define-syntax println
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(begin (print ,(cadr slist)) (newline)))))

; Define ?:
; (?: (< x 0) (- x) x)
(define-syntax ?:
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(if ,(cadr slist) ,(caddr slist) ,(cadddr slist)))))