#lang racket

; Converts a piece of the program into the integer value it represents
(define M_integer
  (lambda (lis)
    (cond
      ((number? lis) lis)
      ((eq? '+ (operator lis)) (+ (M_integer (num1 lis)) (M_integer (num2 lis))))
      ((eq? '- (operator lis)) (- (M_integer (num1 lis)) (M_integer (num2 lis))))
      ((eq? '* (operator lis)) (* (M_integer (num1 lis)) (M_integer (num2 lis))))
      ((eq? '/ (operator lis)) (quotient (M_integer (num1 lis)) (M_integer (num2 lis))))
      ((eq? '% (operator lis)) (remainder (M_integer (num1 lis)) (M_integer (num2 lis)))))))

; Functions to make the code abstract
(define operator car)
(define num1 cadr)
(define num2 caddr)

; longer version; for when operator comes in the middle
; (define operator (lambda (lis) (cadr lis)))
; (define num1 (lambda (lis) (car lis)))
; (define num2 (lambda (lis) (caddr lis)))

; for when operator comes first
; (define operator (lambda (lis) (cadr lis)))
; (define num1 (lambda (lis) (car lis)))
; (define num2 (lambda (lis) (caddr lis)))