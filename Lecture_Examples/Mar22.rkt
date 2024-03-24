#lang racket

; explore recursion
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; build factorial without using the name factorial in the body
; attempt 1: "unroll" the recursion/loop; replace recursive call w/ actual def.
(define crash (lambda (n) (error 'crashed)))
(define factorial1
  (lambda (n)
    (if (zero? n)
        1
        (* n ((lambda (n)
                (if (zero? n)
                    1
                    (* n ((lambda (n)
                            (if (zero? n)
                                1
                                (* n (crash (- n 1)))))
                          (- n 1)))))
              (- n 1))))))

; have the function definition be input to our function framework so that it (almost)
; automatically adds it in as needed
(define factorial2
  ((lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (- n 1))))))
   crash)) ; f was crash, so (factorial2 1) crashes

(define factorial2-1
  ((lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (- n 1))))))
   ((lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n (f (- n 1))))))
    crash))) ; keep adding for higher number inputs...

; Create a framework function for what we are building and pass in both the definition of factorial and
; this "build factorial" framework in as parameters
(define factorial3
  ((lambda (m f) (m (m (m (m f))))) ; can keep adding to f
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (- n 1))))))
   crash))

; no limit! can unroll infinite number of times
; can do recursion without calling a name bound to a function
; Y-combinator; building multiple unrolls
(define factorial4
  ((lambda (m) (m m))
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((f f) (- n 1))))))))

; build a y-combinator for append
(define myappend
  )