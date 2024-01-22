#lang racket

; remove the first occurrence of an atom in a list of atoms
(define myremove
  (lambda (x lis)
    (cond
      [(null? lis) '()] ;returning the empty list
      [(eq? x (car lis)) (cdr lis)]
      [else (cons (car lis) (myremove x (cdr lis)))])))
      ; return a list without the removed atom

; remove all occurrences of an atom in a list of atoms
(define myremoveall
  (lambda (x lis)
    (cond
      [(null? lis) '()] ; returning an empty list
      [(eq? x (car lis)) (myremoveall x (cdr lis))]
      [else (cons (car lis) (myremoveall x (cdr lis)))])))
      ; rebuild the list by recursively putting elements back

; append two lists of atoms into a single list contaiing all atoms of 1st list followed by 2nd list
(define myappend
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (myappend (cdr lis1) lis2)))))

; (repeat 5 'a) => '(a a a a a)
(define repeat
  (lambda (n a)
    (if (zero? n)
        '() ; return an empty list if n is 0
        (cons a (repeat (- n 1) a)))))
    
; (squares '(1 4 6)) => '(1 16 36))
(define squares
  (lambda (lis)
    (if (null? lis)
        '() ; return an empty list if lis is empty
        (cons (* (car lis) (car lis)) (squares (cdr lis))))))
        ; square each 1st element

; (myreplace 'x 'y '(a b x c d x e)) => '(a b y c d x e)
(define myreplace
  (lambda (x y lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (cons y (cdr lis)))
      (else (cons (car lis) (myreplace x y (cdr lis)))))))

; (myreplaceall 'x 'y '(a b x c d x e)) => '(a b y c d y e)
(define myreplaceall
  (lambda (x y lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (cons y (myreplaceall x y (cdr lis))))
      (else (cons (car lis) (myreplaceall x y (cdr lis)))))))


; add an atom to the end of a list
(define add2end
  (lambda (x lis)
    (if (null? lis)
        (cons x '()) ; returns a list containing x at the end
        (cons (car lis) (add2end x (cdr lis))))))
        ; not memory efficient but elegant! => referential transparency

; Referential transparency example
(define fun
  (lambda (lis)
    (myappend (add2end 'x lis) (add2end 'y lis))))
