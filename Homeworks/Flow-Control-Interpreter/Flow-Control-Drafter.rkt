#lang racket
(require "simpleParser.rkt")
(require test-engine/racket-tests)

; Authors: Amir Aliyu, Chae Kim
; Date: Mar 16, 2024
; Description: Flow Control Interpreter


; test by running (test) in console

; test cases for part 1
#|
(check-expect (interpret "MakeTestsPart1/test1a.txt") 150)
(check-expect (interpret "MakeTestsPart1/test2a.txt") -4)
(check-expect (interpret "MakeTestsPart1/test3..txt") 10)
(check-expect (interpret "MakeTestsPart1/test4a.txt") 16)
(check-expect (interpret "MakeTestsPart1/test5a.txt") 220)
(check-expect (interpret "MakeTestsPart1/test6a.txt") 5)
(check-expect (interpret "MakeTestsPart1/test7a.txt") 6)
(check-expect (interpret "MakeTestsPart1/test8a.txt") 10)
(check-expect (interpret "MakeTestsPart1/test9a.txt") 5)
(check-expect (interpret "MakeTestsPart1/test10a.txt") -39)
(check-error (interpret "MakeTestsPart1/test11a.txt") "using before declaring")
(check-error (interpret "MakeTestsPart1/test12a.txt") "using before declaring")
(check-error (interpret "MakeTestsPart1/test13a.txt") "using before assigning")
(check-error (interpret "MakeTestsPart1/test14a.txt") "using before assigning")
(check-expect (interpret "MakeTestsPart1/test15a.txt") 'true)
(check-expect (interpret "MakeTestsPart1/test16a.txt") 100)
(check-expect (interpret "MakeTestsPart1/test17a.txt") 'false)
(check-expect (interpret "MakeTestsPart1/test18a.txt") 'true)
(check-expect (interpret "MakeTestsPart1/test19a.txt") 128)
(check-expect (interpret "MakeTestsPart1/test20a.txt") 12)
|#

; test cases for part 2
;#| <- multi-line comment
(check-expect (interpret "MakeTestsPart2/test1b.txt") 20)
;(check-expect (interpret "MakeTestsPart2/test2b.txt") 164)
;(check-expect (interpret "MakeTestsPart2/test3b.txt") 32)
;(check-expect (interpret "MakeTestsPart2/test4b.txt") 2)
;(check-error (interpret "MakeTestsPart2/test5b.txt") "error")
;(check-expect (interpret "MakeTestsPart2/test6b.txt") 25)
;(check-expect (interpret "MakeTestsPart2/test7b.txt") 21)
;(check-expect (interpret "MakeTestsPart2/test8b.txt") 6)
;(check-expect (interpret "MakeTestsPart2/test9b.txt") -1)
;(check-expect (interpret "MakeTestsPart2/test10b.txt") 789)
;(check-error (interpret "MakeTestsPart2/test11b.txt") "error")
;(check-error (interpret "MakeTestsPart2/test12b.txt") "error")
;(check-error (interpret "MakeTestsPart2/test13b.txt") "error")
;(check-expect (interpret "MakeTestsPart2/test14b.txt") 12)
;(check-expect (interpret "MakeTestsPart2/test15b.txt") 125)
;(check-expect (interpret "MakeTestsPart2/test16b.txt") 110)
;(check-expect (interpret "MakeTestsPart2/test17b.txt") 2000400)
;(check-expect (interpret "MakeTestsPart2/test18b.txt") 101)
;(check-error (interpret "MakeTestsPart2/test19b.txt") "error")
;(check-expect (interpret "MakeTestsPart2/test20b.txt") 21) ; NOTE: NOT REQUIRED - ADDITIONAL PRACTICE
;|#


;============================================================================
; MAIN FUNCTION
;============================================================================

; interpret takes a filename, calls parser with the filename, evaluates the syntax tree
; returned by parser, and returns the proper value.
(define interpret
  (lambda (filename)
    (parse-prog (parser filename) '[((true false) (#t #f))]))) ; the layer contains the initial state (expressed with [])
                                                                              ; the initial state is a list with 2 sublists
                                                                              ; true or false stored as default

; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
(define parse-prog
  (lambda (prog state)
    (if (null? prog)
        (lookup-layers 'return state) ; End of program. Give the return value. ;FIX?
        (m-state (curr-line prog) state (lambda (new-state) (parse-prog (next-lines prog) new-state)))))) ; Parse the next statement

; helper function for next program
(define curr-line (lambda (prog) (if (null? prog) prog (car prog))))
(define next-lines (lambda (prog) (if (null? prog) prog (cdr prog))))

;============================================================================
; STATE FUNCTIONS (i.e. lookup, addbinding, etc.)
;============================================================================

; Helper functions for abstraction
; For individual states; make sure to only input a single state
(define get-vars    (lambda (state) (car state))) ; all vars
(define get-vals    (lambda (state) (cadr state))) ; all vals
(define next-state  (lambda (state) (if (empty-state state) state (list (cdr (car state)) (cdr (cadr state)))))) ; next state
(define empty-state (lambda (state) (if (null? (car state)) #t #f))); return whether state is empty or not
(define get-var     (lambda (state) (car (get-vars state)))) ; get first var; assume null state checked beforehand
(define get-val     (lambda (state) (car (get-vals state)))) ; get first val; assume null state checked beforehand
; For layers
(define curr-layer (lambda (layers) (if (empty-layers layers) layers (car layers))))
(define next-layers (lambda (layers) (if (empty-layers layers) layers (cdr layers)))) ; returns the next state on layers
(define next-curr-layer (lambda (layers) (if (or (null? layers) (null? (curr-layer layers))) layers ; next in 1st layer
                                               (cons (next-state (curr-layer layers)) (cdr layers)))))
(define empty-layers (lambda (layers) (if (null? layers) #t #f))) ; end of the list of layers reached

; lookup function; return value of var or if it is not declared yet (error) or novalue (for single-layered states in pt. 1)
(define lookup
  (lambda (var state)
    (cond
      ((empty-state state)          (error "using before declaring")) ; var not initialized yet ; FIX?
      ((empty-state state)          state)
      ((equal? (get-var state) var) (get-val state)) ; returns value or 'novalue depending on assignment status
      (else                         (lookup var (next-state state)))))) ; not equal; recurse down further

; lookup function w/ layer should traverse all layers in order and return/update first match found
(define lookup-layers
  (lambda (var layers)
    (cond
      ((empty-layers layers)                      (error "using before declaring"))
      ((empty-state (curr-layer layers))          (lookup-layers var (next-layers layers))) ; current state is empty
      ((equal? (get-var (curr-layer layers)) var) (get-val (curr-layer layers))) ; current layer has a variable that matches the input
      (else                                       (lookup-layers var (next-curr-layer layers)))))) ; continue in current layer

; new state using layers should look like:
; '(((x y z) (1 2 3)) ((a) (2)))
; if the block w/ x y z is exited:
; '(((a) (2)))

; helper function to see if a variable already exists in layers/states
(define findVar-layers
  (lambda (var layers)
    (cond
      ((empty-layers layers) #f)
      ((empty-state (curr-layer layers)) (findVar-layers var (next-layers layers)))
      ((equal? (get-var (curr-layer layers)) var) #t)
      (else (findVar-layers var (next-curr-layer layers))))))

; helper function to return t/f depending on whether var already exists in current state/layer
(define findVar
  (lambda (var state)
    (cond
      ((empty-state state) #f)
      ((equal? (get-var state) var) #t)
      (else (findVar var (next-state state))))))

; addbinding adds a binding to the state
(define addbinding
  (lambda (var val state)
    (list (cons var (get-vars state)) (cons val (get-vals state))))) ; adding to state

(define addbinding-layers
  (lambda (var val layers)
    (cond
      ((empty-layers layers) layers)
      ((false? (findVar-layers var layers)) ; var not in the layers yet; add to the top layer
       (cons (addbinding var val (curr-layer layers)) (rmv-layer layers)))
      (else (cons (curr-layer layers) (updatebinding-layers var val (next-layers layers))))))) ; update existing binding if exists

; removebinding removes a binding from the state
(define removebinding
  (lambda (var state)
    (cond
      ((empty-state state)          state) ; state is empty; return original state
      ((equal? (get-var state) var) (next-state state)) ; found var; remove binding
      (else                         (addbinding (get-var state) ; move onto the next
                                                (get-val state)
                                                (removebinding var (next-state state)))))))

; removebinding for multiple layers
(define removebinding-layers
  (lambda (var layers)
    (cond
      ((empty-layers layers) layers)
      ((false? (findVar var (curr-layer layers))) (cons (curr-layer layers) (removebinding-layers var (next-layers layers))))
      (else (cons (removebinding var (curr-layer layers)) (rmv-layer layers))))))

; updatebinding adds a binding if it does not already exist; if a binding exists, it updates.
(define updatebinding
  (lambda (var val state)
    (addbinding var val (removebinding var state)))) ;remove existing bindings first & add new binding

; updatebinding for layers
(define updatebinding-layers
  (lambda (var val layers)
    (cond
      ((empty-layers layers) layers)
      ((false? (findVar var (curr-layer layers))) (cons (curr-layer layers) (updatebinding-layers var val (next-layers layers))))
      (else (cons (updatebinding var val (curr-layer layers)) (rmv-layer layers))))))


;============================================================================
; EVAL FUNCTIONS (i.e. m-state, m-int, m-bool, etc.)
;============================================================================

; abstractions for simple statements i.e.:
; '(+ 3 (- 3 5)) => operator: +
;                   loperand: 3
;                   roperand: (- 3 5)
(define operator car)
(define loperand cadr)
(define roperand caddr)

; abstraction for extracing arguments i.e.:
; '(operator 3 5 8) => arg1: 3
;                      arg2: 5
;                      arg3: 8
(define arg1 cadr)
(define arg2 caddr)
(define arg3 cadddr)

; m-bool takes a syntax rule and a state and produces a true / false value (or an error condition).
(define m-bool 
  (lambda (boolexp state)
    (cond
      ((number? boolexp)             boolexp)
      ((symbol? boolexp)            (if (equal? (lookup-layers boolexp state) 'novalue)
                                        (error "used before assigned")
                                        (lookup-layers boolexp state)))
      ((intexp? boolexp)            (m-int boolexp state)) ; handle intexps nested in boolean expressions
      ((or (is-asgn (loperand boolexp)) ; left or right operand is an assignment stmt
           (and (roperand? boolexp) (is-asgn (roperand boolexp))))
       (m-bool (new-stmt (operator boolexp) (value-get (asgn-var (loperand boolexp)) (m-state (loperand boolexp) state (lambda (v) v)))
                        (value-get (asgn-var (roperand boolexp)) (m-state (roperand boolexp) (m-state (loperand boolexp) state (lambda (v) v)) (lambda (v) v))))
               (m-state (roperand boolexp) (m-state (loperand boolexp) state))))
      ((eq? (operator boolexp) '&&) (and (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '||) (or  (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '!)  (not (m-bool (arg1 boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '==) (equal? (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '!=) (not (equal? (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state (lambda (v) v))))))
      ((eq? (operator boolexp) '<)  (<   (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '>)  (>   (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '<=) (<=  (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state (lambda (v) v)))))
      ((eq? (operator boolexp) '>=) (>=  (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state (lambda (v) v))))))))

(define asgn-var (lambda (stmt) (if (not (list? stmt)) stmt (arg1 stmt)))) ; returns the var of asgn
(define new-stmt (lambda (oper left right) (list oper left right))) ; new statement w/o assignments
(define is-asgn (lambda (stmt) (if (and (list? stmt) (eq? (operator stmt) '=)) #t #f))) ; returns whether stmt is assignment or not
 

; intexp? determines if an expression is an intexp, i.e. does it start with an arithmetic operator?
(define intexp?
  (lambda (exp)
    (or (eq? (operator exp) '+)
        (eq? (operator exp) '-)
        (eq? (operator exp) '*)
        (eq? (operator exp) '/)
        (eq? (operator exp) '%))))

; boolexp? determines if an expression is a boolexp
(define boolexp?
  (lambda (exp)
    (or (eq? (operator exp) '&&)
        (eq? (operator exp) '||)
        (eq? (operator exp) '!)
        (eq? (operator exp) '==)
        (eq? (operator exp) '!=)
        (eq? (operator exp) '<)
        (eq? (operator exp) '>)
        (eq? (operator exp) '<=)
        (eq? (operator exp) '>=))))

(define roperand? (lambda (stmt) (if (null? (cddr stmt)) #f #t))) ; true if no 2nd operand
        
; m-int takes in an expression and a state and returns a numeric value
(define m-int 
  (lambda (intexp state)
    (cond
      ((number? intexp)           intexp)
      ((symbol? intexp)           (if (equal? (lookup-layers intexp state) 'novalue)
                                      (error "using before assigning")
                                      (lookup-layers intexp state))) ; lookup variable value
      ((or (is-asgn (loperand intexp)) ; left or right operand is an assignment stmt
           (and (roperand? intexp) (is-asgn (roperand intexp))))
       (m-int (new-stmt (operator intexp) (value-get (asgn-var (loperand intexp)) (m-state (loperand intexp) state (lambda (v) v)))
                        (value-get (asgn-var (roperand intexp)) (m-state (roperand intexp) (m-state (loperand intexp) state (lambda (v) v)) (lambda (v) v)))) state))
      ((and (eq? (operator intexp) '-) (unary? intexp)) (* '-1 (m-int (loperand intexp) (m-state (loperand intexp) state (lambda (v) v)))))
      ((eq? (operator intexp) '+) (+ (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state (lambda (v) v)))))
      ((eq? (operator intexp) '-) (- (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state (lambda (v) v)))))
      ((eq? (operator intexp) '*) (* (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state (lambda (v) v)))))
      ((eq? (operator intexp) '/) (quotient (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state (lambda (v) v)))))
      ((eq? (operator intexp) '%) (remainder (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state (lambda (v) v))))))))
 
; Helper function for checking whether the expression is unary or not
(define unary? (lambda (exp) (if (null? (cdr (cdr exp))) #t #f)))
(define value-get (lambda (stmt state) (if (or (number? stmt) (boolean? stmt)) stmt (lookup stmt state)))) ; return value of input

; m-state changes the state
(define m-state
  (lambda (stmt state next)
    (cond
      ((not (list? stmt))            (next state))
      ((eq? (operator stmt) '=)      (next (parse-asgn (arg1 stmt) (arg2 stmt) state)))
      ((eq? (operator stmt) 'var)    (next (parse-decl stmt state)))
      ((eq? (operator stmt) 'while)  (next (parse-while stmt state)))
      ((eq? (operator stmt) 'return) (next (parse-return stmt state)))
      ((eq? (operator stmt) 'if)     (next (parse-if stmt state)))
      ((eq? (operator stmt) 'begin)  (next (parse-block stmt state))) ; For a block of code
      ((not (roperand? stmt)) (next (m-state (loperand stmt) state next))) ; no right operand
      (else (next (m-state (roperand stmt) (m-state (loperand stmt) state next) next)))))) ; Else, it's m-int or m-bool with args to update


; bool-check checks if boolean operation is done (returns bool)
(define bool-check
  (lambda (oper)
    (cond
      ((equal? oper '&&) #t)
      ((equal? oper '||) #t)
      ((equal? oper '! ) #t)
      ((equal? oper '==) #t)
      ((equal? oper '!=) #t)
      ((equal? oper '< ) #t)
      ((equal? oper '> ) #t)
      ((equal? oper '<=) #t)
      ((equal? oper '>=) #t)
      (else #f))))

; int-check checks if arithmetic operation is done (returns int)
(define int-check
  (lambda (oper)
    (cond
      ((equal? oper '+) #t)
      ((equal? oper '-) #t)
      ((equal? oper '*) #t)
      ((equal? oper '/) #t)
      ((equal? oper '%) #t)
      (else #f))))

; parse-asgn takes in two statements and a state and adds the binding of the new value to the state 
(define parse-asgn ; assuming input names valid
  (lambda (var expr state)
    (cond
      ((equal? (lookup-layers var state) 'not-initialized) (error "Variable not declared yet")) ; var not declared yet
      ((number? expr)                      (updatebinding-layers var expr state)) ; assigning int
      ((boolean? expr)                     (updatebinding-layers var (bool-cvt expr) state)) ; assigning bool
      ((not (list? expr))                  (updatebinding-layers var (lookup-layers expr state) state)) ; variable
      ((bool-check (operator expr))        (updatebinding-layers var (bool-cvt (m-bool expr state))
                                                       (m-state expr state (lambda (v) v)))) ; bool expr
      ((int-check (operator expr))         (updatebinding-layers var (m-int expr state) ; numerical expression
                                                       (m-state expr state (lambda (v) v)))) 
      (else                                (updatebinding-layers var ; handle assignment within assignment
                                                       (lookup-layers (arg1 expr) (parse-asgn (arg1 expr) (arg2 expr) state))
                                                       (parse-asgn (arg1 expr) (arg2 expr) state))))))

; exp-arg returns true if assignment with declaration, else false
(define exp-arg (lambda (stmt) (if (null? (decl-exp stmt)) #f #t)))
; For converting boolean (#t/f) to true or false
(define bool-cvt (lambda (expr) (cond ((eq? #t expr) 'true) ((eq? #f expr) 'false) (expr))))

; abstraction for exp-arg helper
(define decl-exp
  (lambda (stmt)
    (cdr (cdr stmt))))

; parse-decl parses declarations--
; takes a declaration statement and adds it to the state with atom 'novalue to show it hasnt been given a value
(define parse-decl
  (lambda (stmt state)
    (cond
      ((not (exp-arg stmt)) (addbinding-layers (arg1 stmt) 'novalue state)) ; no assignment
      (else (parse-asgn (arg1 stmt) (arg2 stmt) (addbinding-layers (arg1 stmt) 'novalue state))))))

; parse-if parses if statements
(define parse-if
  (lambda (stmt state)
    (cond
      ((m-bool (cond-stmt stmt) state)    (m-state (then-stmt stmt)  ; conditional statement is true
                                                   (m-state (cond-stmt stmt) state (lambda (v) v)) (lambda (v) v)))
      ((equal? 'no-else (else-stmt stmt)) (m-state (cond-stmt stmt) state (lambda (v) v))) ; conditional false and no else; no action
      ((nested-if (else-stmt stmt))       (parse-if (else-stmt stmt) ; nested if statements
                                                    (m-state (cond-stmt stmt) state (lambda (v) v))))
      (else                               (m-state (else-stmt stmt)  ; no nested if statements
                                                   (m-state (cond-stmt stmt) state (lambda (v) v)) (lambda (v) v))))))

; Helper functions for if statements to help with abstraction
(define cond-stmt (lambda (stmt) (car (cdr stmt)))); conditional statement
(define then-stmt (lambda (stmt) (car (cdr (cdr stmt))))) ; then statement
(define else-stmt (lambda (stmt) ; optional else statement
                    (if (null? (cdr (cdr (cdr stmt)))) 'no-else
                        (car (cdr (cdr (cdr stmt)))))))

; Takes in else-stmt and determine if there's a nested if statement
(define nested-if
  (lambda (else-stmt)
    (cond
      ((null? else-stmt) 'no-else)
      ((equal? 'if (car else-stmt)) #t)
      (else #f))))

; parse-while parses a while statement
(define parse-while
  (lambda (stmt state)
    (cond
      ((m-bool (condition stmt) state) ; condition is true; continue looping
       (parse-while stmt (m-state (statement stmt) (m-state (condition stmt) state (lambda (v) v)) (lambda (v) v))))
      (else (m-state (condition stmt) state (lambda (v) v)))))) ; condition is false; stop loop

; abstraction for while statement
(define condition cadr)
(define statement caddr)

; parse-return parses a return statement
(define parse-return
  (lambda (stmt state)
    (parse-asgn 'return (return-val stmt) (addbinding-layers 'return 'novalue state))))

; Helper function for parse-return; return the expression or value to be returned
(define return-val (lambda (stmt) (car (cdr stmt))))



;============================================================================
; FLOW CONTROL FUNCTIONS (Part 2)
;============================================================================

; Parsing code blocks using call-cc
(define parse-block
  (lambda (stmt layers) ; assume layers are inputted
    (cond
      ;((empty-layers layers) (error "no states given")) ; no state in layers
      ((empty-stmt stmt) (rmv-layer layers)) ; remove the current layer at the end of block
      ((eq? 'begin (curr-stmt stmt)) (parse-block (next-stmts stmt) (add-layer layers)))
      (else (m-state (curr-stmt stmt) layers (lambda (state) (parse-block (next-stmts stmt) state)))))))

; Helper function to return next statement in the block
(define curr-stmt (lambda (stmt) (if (null? stmt) stmt (car stmt))))
(define empty-stmt (lambda (stmt) (if (null? stmt) #t #f)))
(define next-stmts (lambda (stmt) (if (null? (cdr stmt)) stmt (cdr stmt))))
(define add-layer (lambda (layers) (cons '(() ()) layers)))
(define rmv-layer (lambda (layers) (cdr layers)))



