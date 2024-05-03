#lang racket

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; try creating a function where its name is not mentioned inside its body
(define crash (lambda (n) (error 'crash)))

(define fact1
  (lambda (n)
    (if (zero? n)
        1
        (* n ((lambda (n)
                (if (zero? n)
                    1
                    (* n (crash (- n 1)))))
              (- n 1))))))

; keep replacing crash with the function; extending
; this way, "crash" becomes just a placeholder
(define fact1-1
  (lambda (n) ; each of these n's are different; ok b/c bound correctly
    (if (zero? n)
        1
        (* n ((lambda (n)
                (if (zero? n)
                    1
                    (* n ((lambda (n)
                            (if (zero? n)
                                1
                                (* 1 ((lambda (n)
                                        (if (zero? n)
                                            1
                                            (crash (- n 1))))
                                      (- n 1)))))
                          (- n 1)))))
              (- n 1))))))
; this is "unrolling" the recursion by replacing each recursive call with the code that is the function being called
; not actually doing recursion; just unrolling
; physically hard-coding in the same thing; doing the same thing every time

; since we're doing the exact same thing every time, pass in the function as an input parameter
; since bound to the input variable, it is not actually defined
(define fact2
  ((lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (- n 1))))))
   crash))

; to replace the 'crash' above, we can input another function
(define fact2-1
  ((lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (- n 1))))))
   (lambda (n)
     (if (zero? n)
         1
         (* n (crash (- n 1)))))))
; but this is just unrolling as before; not much better
; therefore, run the lambda f code above; pass in the function each time
(define fact3
  ((lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (- n 1))))))
   ((lambda (f) ; this and below is the input function 'f' for above
      (lambda (n)
        (if (zero? n)
            1
            (* n (f (- n 1))))))
    ((lambda (f)
       (lambda (n)
         (if (zero? n)
             1
             (* n (f (- n 1))))))
     crash))))
; still unrolling a loop, but we are passing the body of the function we need to call in as a parameter
; that we use for the "recursion" (not actually recursion b/c not calling a name)
; still doing the same thing over and over

; again, "unrolling" the recursion, but mkf is "building" each unrolled function call automatically
(define fact4
  ((lambda (mkf) (mkf (mkf (mkf (mkf crash))))) ; keep adding; automatic unrolling
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (- n 1))))))))
; still manual b/c it does not have a name

; calling a function on itself?
(define fact5
  ((lambda (mkf) (mkf mkf)) ; keep adding; automatic unrolling
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((f f) (- n 1)))))))) ; giving the same function itself again in this line
; a function has an access to itself! this is true definition of recursion
; giving it a name is how modern languages give functions access to itself

; example: length function
(((lambda (m) (m m))
  (lambda (f)
    (lambda (l)
      (if (null? l)
          0
          (+ 1 ((f f) (cdr l))))))) '(a b c d e f))

;; Y-Combinator Practice

; myappend
(define myappend
  ((lambda (f) (f f)) ; call the later function with itself as the input
   (lambda (func) ; here again, it's giving access to itself with parameter binding
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((func func) (cdr l1) l2)))))))


; myreverse
(define myreverse
  ((lambda (rev-func app-func) (rev-func rev-func app-func))
   (lambda (reverse append) ; first input function (reverse)
     (lambda (l)
       (if (null? l)
           l
           ((append append) ((reverse reverse append) (cdr l)) (list (car l))))))
   (lambda (append) ; second input function (append)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((append append) (cdr l1) l2)))))))


; original functions
(define myappend-orig
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend-orig (cdr l1) l2)))))

(define myreverse-orig
  (lambda (l)
    (if (null? l)
        l
        (myappend-orig (myreverse-orig (cdr l)) (list (car l))))))
