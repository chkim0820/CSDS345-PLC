#lang racket
(require "simpleParser.rkt")

; Authors: Alex Wang, Amir Aliyu, Chae Kim
; Date: February 16, 2024
; Description: Simple Interpreter Code

;============================================================================
; MAIN FUNCTION
;============================================================================

; This main function (should be called interpret) takes a filename, calls parser with the filename, evaluates the syntax tree
; returned by parser, and returns the proper value. The state variable is used to represent the
; state of the variables, and an error is returned if a variable is used before it is declared.
; The Scheme's error function is used to return the error with the code: (error "error message").
; The state stores variable-value pairs like this: ((x y z) (1 2 3)), where x = 1, y = 2, z = 3.
; The state should be abstracted away from the rest of the interpreter (whatever that means).

; Note about the state function vs the m-int, m-bool etc functions:
; In this case, that means that the functions that implement the denotational semantics (m-int, m-bool, etc)
; do not know about the structure of the state. Instead, you have generic functions that your interpreter can call 
; to manipulate the state. (means need addBinding, removeBinding) - state shouldnt be manipulated in
; m-int, m-bool etc


; FOR DEBUGGING
(define tree
  (lambda (filename)
    (parser filename)))

(define interpret
  (lambda (filename)
    (parse-prog (parser filename) '(()())))) ; the initial state is a list with 2 sublists
                                             ; because there are no variables or values yet.

; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
; A syntax tree is represented by a list where each sublist corresponds to a statement:
;
; 1. variable declaration (var variable) or (var variable value)
; 2. assignment           (= variable expression)
; 3. return               (return expression)
; 4. if statement         (if conditional then-statement optional-else-statement)
; 5. while statement      (while conditional body-statement)
(define parse-prog
  (lambda (prog state)
    (if (null? prog)
        (lookup 'return state) ; End of program. Give the return value.
        (parse-prog (cdr prog) (m-state (car prog) state))))) ; Parse the next statement

;============================================================================
; STATE FUNCTIONS (i.e. lookup, addbinding, etc.)
;============================================================================

; The state is a list with two sublists.
; the first sublist is a list of declared vars
; the second sublist is a list of the corresponding values
; For example, a state with declared vars x=1, y=2, z=3:
; '((x y z) (1 2 3))
; Or, an empty state, for example:
; '(() ())

; Helper functions for abstraction
(define get-vars    (lambda (state) (car state))) ; all vars
(define get-vals    (lambda (state) (cadr state))) ; all vals
(define next-state  (lambda (state) (if (empty-state state) state (list (cdr (car state)) (cdr (cadr state)))))) ; next state
(define empty-state (lambda (state) (if (null? (car state)) #T #F))); return whether state is empty or not
(define get-var     (lambda (state) (car (get-vars state)))) ; get first var; assume null state checked beforehand
(define get-val     (lambda (state) (car (get-vals state)))) ; get first val; assume null state checked beforehand

; lookup function; return value of var or if it is not declared yet (error) or novalue
(define lookup
  (lambda (var state)
    (cond
      ((empty-state state)          'novalue) ; var not declared yet; no more var
      ((equal? (get-var state) var) (get-val state)) ; returns value or 'novalue depending on assignment status
      (else                         (lookup var (next-state state)))))) ; not equal; recurse down further

; lookup? checks if a variable exists
(define lookup?
  (lambda (var state)
    (cond
      ((null? state) #f) ; var not in state
      ((eq?          (get-var state) var) #t) ; var found; return true
      (else          (lookup? var (next-state state)))))) ; continue to recurse down

; addbinding function
(define addbinding
  (lambda (var val state)
    (list (cons var (get-vars state)) (cons val (get-vals state))))) ; adding to state

; removebinding function
(define removebinding
  (lambda (var state)
    (cond
      ((empty-state state)          state) ; state is empty; return original state
      ((equal? (get-var state) var) (next-state state)) ; found var; remove binding
      ((addbinding (get-var state)  (get-val state) (removebinding var (next-state state))))))) ; move onto the next


; updatebinding function
; updatebinding adds a binding if it does not already exist; if a binding exists, it updates.
(define updatebinding
  (lambda (var val state)
    (addbinding var val (removebinding var state)))) ;remove existing bindings first & add new binding

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
; this implementation of m-bool returns #t or #f
; m-bool takes a syntax rule and a state and produces a true / false value (or an error condition).
(define m-bool
  (lambda (boolexp state)
    (cond
      ((number? boolexp)             boolexp)
      ((symbol? boolexp)            (lookup boolexp state))
      ((intexp? boolexp)            (m-int boolexp state)) ; handle intexps nested in boolean expressions
      ((eq? (operator boolexp) '&&) (and (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) state)))
      ((eq? (operator boolexp) '||) (or  (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) state)))
      ((eq? (operator boolexp) '!)  (not (m-bool (arg1 boolexp) state)))
      ((eq? (operator boolexp) '==) (eq? (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) state)))
      ((eq? (operator boolexp) '<)  (<   (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) state)))
      ((eq? (operator boolexp) '>)  (>   (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) state)))
      ((eq? (operator boolexp) '<=) (<=  (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) state)))
      ((eq? (operator boolexp) '>=) (>=  (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) state))) )))

; determines if an expression is an intexp, i.e. does it start with an arithmetic operator?
(define intexp?
  (lambda (exp)
    (or (eq? (operator exp) '+)
        (eq? (operator exp) '-)
        (eq? (operator exp) '*)
        (eq? (operator exp) '/)
        (eq? (operator exp) '%))))

; determines if an expression is a boolexp
(define boolexp?
  (lambda (exp)
    (or (eq? (operator exp) '&&)
        (eq? (operator exp) '||)
        (eq? (operator exp) '!)
        (eq? (operator exp) '==)
        (eq? (operator exp) '<)
        (eq? (operator exp) '>)
        (eq? (operator exp) '<=)
        (eq? (operator exp) '>=))))
        
; m-int takes in an expression and a state and returns a numeric value
(define m-int
  (lambda (intexp state)
    (cond
      ((number? intexp)           intexp)
      ((symbol? intexp)           (lookup intexp state)) ; lookup variable value
      ((eq? (operator intexp) '+) (+ (m-int (loperand intexp) state) (m-int (roperand intexp) state)))
      ((eq? (operator intexp) '-) (- (m-int (loperand intexp) state) (m-int (roperand intexp) state)))
      ((eq? (operator intexp) '*) (* (m-int (loperand intexp) state) (m-int (roperand intexp) state)))
      ((eq? (operator intexp) '/) (quotient (m-int (loperand intexp) state) (m-int (roperand intexp) state)))
      ((eq? (operator intexp) '%) (remainder (m-int (loperand intexp) state) (m-int (roperand intexp) state))))))

; state should call parse-decl, parse-asgn, etc based on which operation is necessary (i think?)
; and then return the changed state
; Also need to implement: 
; looking up a binding, creating a new binding, and updating an existing binding

; proposing to change 'state' to be called m-state, assuming below functions to be 
; for changing state 
(define m-state
  (lambda (stmt state)
    (cond
      ((eq? (operator stmt) '=)      (parse-asgn (arg1 stmt) (arg2 stmt) state))
      ((eq? (operator stmt) 'var)    (parse-decl stmt state))
      ((eq? (operator stmt) 'while)  (parse-while stmt state))
      ((eq? (operator stmt) 'return) (parse-return stmt state))
      ((eq? (operator stmt) 'if)     (parse-if stmt state)))))

; check if boolean operation is done (returns bool)
(define bool-check
  (lambda (oper)
    (cond
      ((equal? oper '&&) #t)
      ((equal? oper '||) #t)
      ((equal? oper '! ) #t)
      ((equal? oper '==) #t)
      ((equal? oper '< ) #t)
      ((equal? oper '> ) #t)
      ((equal? oper '<=) #t)
      ((equal? oper '>=) #t)
      (else #f))))

; check if arithmetic operation is done (returns int)
(define int-check
  (lambda (oper)
    (cond
      ((equal? oper '+) #t)
      ((equal? oper '-) #t)
      ((equal? oper '*) #t)
      ((equal? oper '/) #t)
      ((equal? oper '%) #t)
      (else #f))))

; Parse an assignment statement.
; takes in two statements and a state and adds the binding of the new value to the state 
(define parse-asgn ; assuming input names valid
  (lambda (var expr state)
    (cond
      ((not (lookup? var state))           'error) ; var not declared yet
      ((or (number? expr) (boolean? expr)) (addbinding var expr (removebinding var state))) ; assigning int or boolean
      ((not (list? expr))                  (addbinding var (lookup expr state) (removebinding var state))) ; variable
      ((bool-check (operator expr))        (addbinding var (m-bool expr state) (removebinding var state))) ; bool expr
      ((int-check (operator expr))         (addbinding var (m-int expr state) (removebinding var state)))  ; num. expr
      (else                                (addbinding var ; handle assignment within assignment
                                                       (lookup (arg1 expr) (parse-asgn (arg1 expr) (arg2 expr) state))
                                                       (parse-asgn (arg1 expr) (arg2 expr) (removebinding var state)))))))

; For declaration; true if no assignment with declaration, else false
(define exp-arg (lambda (stmt) (if (null? (cdr (cdr stmt))) #f #t)))

; parse-decl should take a declaration statement and add it to the state with atom 'novalue to show it hasnt been given a value
(define parse-decl
  (lambda (stmt state)
    (cond
      ((not (exp-arg stmt)) (addbinding (arg1 stmt) 'novalue state))
      ((parse-asgn (arg1 stmt) (arg2 stmt) (addbinding (arg1 stmt) 'novalue state))))))

; takes in a statement and a state, returns changed state if the condition is true
(define parse-if 
  (lambda (stmt state)
    (cond
      ; if the condition is true, execute the then-statement
      ((eq? #t (m-bool (if-cond stmt) state)) (m-state (then-stmt stmt) state))
      ; if the condition is false and there is an else-statement, execute it
      ((eq? #t (has-else stmt)) (m-state (else-stmt stmt) state))
      ; if the condition is false and there is no else-statement, do nothing
      (else state))))

; abstraction for parse-if:
(define if-cond
  (lambda (stmt)
    (car (cdr stmt))))

(define then-stmt
  (lambda (stmt)
    (car (cdr (cdr stmt)))))

(define else-stmt
  (lambda (stmt)
    (car (cdr (cdr (cdr stmt))))))

; check if else statement exists
(define has-else
  (lambda (stmt)
    (pair? (cdr (cdr (cdr stmt))))))

; abstractions for while statement
(define condition cadr)
(define statement caddr)

; parses a while statement
(define parse-while
  (lambda (stmt state)
    (if (m-bool (condition stmt) state)
        (m-state stmt (m-state (statement stmt) state))
        state)))

; Parses a return statement
(define parse-return
  (lambda (stmt state)
    (parse-asgn 'return (return-val stmt) (addbinding 'return 'novalue state))))

;(define parse-return
;  (lambda (stmt state)
;    (cond
;      ((or (number? (return-val stmt)) (boolean? (return-val stmt))) (addbinding 'return (return-val stmt) (removebinding 'return state))) ; int or boolean
;      ((not (list? (return-val stmt))) (addbinding 'return (lookup (return-val stmt) state) (removebinding 'return state))) ; variable
;      ((bool-check (operator (return-val stmt))) (addbinding 'return (m-bool (return-val stmt) state) (removebinding 'return state))) ; bool op
;      ((int-check (operator (return-val stmt))) (addbinding 'return (m-int (return-val stmt) state) (removebinding 'return state)))
;      (state)))) ; num oper

(define return-val
  (lambda (stmt)
    (car (cdr stmt))))