#lang racket
(require "simpleParser.rkt")

; Authors: Alex Wang, Amir Aliyu, Chae Kim
; Date: February 16, 2024
; Description: Simple Interpreter Code

;============================================================================
; MAIN FUNCTION
;============================================================================

; interpret takes a filename, calls parser with the filename, evaluates the syntax tree
; returned by parser, and returns the proper value.
(define interpret
  (lambda (filename)
    (parse-prog (parser filename) '((true false) (#t #f))))) ; the initial state is a list with 2 sublists
                                                             ; true or false stored as default

; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
(define parse-prog
  (lambda (prog state)
    (if (null? prog)
        (lookup 'return state) ; End of program. Give the return value.
        (parse-prog (cdr prog) (m-state (car prog) state))))) ; Parse the next statement

;============================================================================
; STATE FUNCTIONS (i.e. lookup, addbinding, etc.)
;============================================================================

; Helper functions for abstraction
(define get-vars    (lambda (state) (car state))) ; all vars
(define get-vals    (lambda (state) (cadr state))) ; all vals
(define next-state  (lambda (state) (if (empty-state state) state (list (cdr (car state)) (cdr (cadr state)))))) ; next state
(define empty-state (lambda (state) (if (null? (car state)) #t #f))); return whether state is empty or not
(define get-var     (lambda (state) (car (get-vars state)))) ; get first var; assume null state checked beforehand
(define get-val     (lambda (state) (car (get-vals state)))) ; get first val; assume null state checked beforehand

; lookup function; return value of var or if it is not declared yet (error) or novalue
(define lookup
  (lambda (var state)
    (cond
      ((empty-state state)          (error "using before declaring")) ; var not initialized yet
      ((equal? (get-var state) var) (get-val state)) ; returns value or 'novalue depending on assignment status
      (else                         (lookup var (next-state state)))))) ; not equal; recurse down further

; addbinding adds a binding to the state
(define addbinding
  (lambda (var val state)
    (list (cons var (get-vars state)) (cons val (get-vals state))))) ; adding to state

; removebinding removes a binding from the state
(define removebinding
  (lambda (var state)
    (cond
      ((empty-state state)          state) ; state is empty; return original state
      ((equal? (get-var state) var) (next-state state)) ; found var; remove binding
      (else                         (addbinding (get-var state) ; move onto the next
                                                (get-val state)
                                                (removebinding var (next-state state)))))))

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
(define m-bool 
  (lambda (boolexp state)
    (cond
      ((number? boolexp)             boolexp)
      ((symbol? boolexp)            (if (equal? (lookup boolexp state) 'novalue)
                                        (error "used before assigned")
                                        (lookup boolexp state)))
      ((intexp? boolexp)            (m-int boolexp state)) ; handle intexps nested in boolean expressions
      ((or (is-asgn (loperand boolexp)) ; left or right operand is an assignment stmt
           (and (roperand? boolexp) (is-asgn (roperand boolexp))))
       (m-bool (new-stmt (operator boolexp) (value-get (asgn-var (loperand boolexp)) (m-state (loperand boolexp) state))
                        (value-get (asgn-var (roperand boolexp)) (m-state (roperand boolexp) (m-state (loperand boolexp) state))))
               (m-state (roperand boolexp) (m-state (loperand boolexp) state))))
      ((eq? (operator boolexp) '&&) (and (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '||) (or  (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '!)  (not (m-bool (arg1 boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '==) (equal? (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '!=) (not (equal? (m-bool (arg1 boolexp) state) (m-bool (arg2 boolexp) (m-state (arg1 boolexp) state)))))
      ((eq? (operator boolexp) '<)  (<   (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '>)  (>   (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '<=) (<=  (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state))))
      ((eq? (operator boolexp) '>=) (>=  (m-bool (loperand boolexp) state) (m-bool (roperand boolexp) (m-state (arg1 boolexp) state)))))))

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
      ((symbol? intexp)           (if (equal? (lookup intexp state) 'novalue)
                                      (error "using before assigning")
                                      (lookup intexp state))) ; lookup variable value
      ((or (is-asgn (loperand intexp)) ; left or right operand is an assignment stmt
           (and (roperand? intexp) (is-asgn (roperand intexp))))
       (m-int (new-stmt (operator intexp) (value-get (asgn-var (loperand intexp)) (m-state (loperand intexp) state))
                        (value-get (asgn-var (roperand intexp)) (m-state (roperand intexp) (m-state (loperand intexp) state)))) state))
      ((and (eq? (operator intexp) '-) (unary? intexp)) (* '-1 (m-int (loperand intexp) (m-state (loperand intexp) state))))
      ((eq? (operator intexp) '+) (+ (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state))))
      ((eq? (operator intexp) '-) (- (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state))))
      ((eq? (operator intexp) '*) (* (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state))))
      ((eq? (operator intexp) '/) (quotient (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state))))
      ((eq? (operator intexp) '%) (remainder (m-int (loperand intexp) state) (m-int (roperand intexp) (m-state (loperand intexp) state)))))))
 
; Helper function for checking whether the expression is unary or not
(define unary? (lambda (exp) (if (null? (cdr (cdr exp))) #t #f)))
(define value-get (lambda (stmt state) (if (or (number? stmt) (boolean? stmt)) stmt (lookup stmt state)))) ; return value of input

; m-state changes the state
(define m-state
  (lambda (stmt state)
    (cond
      ((not (list? stmt))            state)
      ((eq? (operator stmt) '=)      (parse-asgn (arg1 stmt) (arg2 stmt) state))
      ((eq? (operator stmt) 'var)    (parse-decl stmt state))
      ((eq? (operator stmt) 'while)  (parse-while stmt state))
      ((eq? (operator stmt) 'return) (parse-return stmt state))
      ((eq? (operator stmt) 'if)     (parse-if stmt state))
      ((not (roperand? stmt)) (m-state (loperand stmt) state)) ; no right operand
      (else (m-state (roperand stmt) (m-state (loperand stmt) state)))))) ; Else, it's m-int or m-bool with args to update

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
      ((equal? (lookup var state) 'not-initialized) (error "Variable not declared yet")) ; var not declared yet
      ((number? expr) (updatebinding var expr state)) ; assigning int
      ((boolean? expr) (updatebinding var (bool-cvt expr) state)) ; assigning bool
      ((not (list? expr))                  (updatebinding var (lookup expr state) state)) ; variable
      ((bool-check (operator expr))        (updatebinding var (bool-cvt (m-bool expr state))
                                                       (m-state expr state))) ; bool expr
      ((int-check (operator expr))         (updatebinding var (m-int expr state) ; numerical expression
                                                       (m-state expr state))) 
      (else                                (updatebinding var ; handle assignment within assignment
                                                       (lookup (arg1 expr) (parse-asgn (arg1 expr) (arg2 expr) state))
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
      ((not (exp-arg stmt)) (addbinding (arg1 stmt) 'novalue state)) ; no assignment
      (else (parse-asgn (arg1 stmt) (arg2 stmt) (addbinding (arg1 stmt) 'novalue state))))))

; parse-if parses if statements
(define parse-if
  (lambda (stmt state)
    (cond
      ((m-bool (cond-stmt stmt) state)    (m-state (then-stmt stmt)  ; conditional statement is true
                                                   (m-state (cond-stmt stmt) state)))
      ((equal? 'no-else (else-stmt stmt)) (m-state (cond-stmt stmt) state)) ; conditional false and no else; no action
      ((nested-if (else-stmt stmt))       (parse-if (else-stmt stmt) ; nested if statements
                                                    (m-state (cond-stmt stmt) state)))
      (else                               (m-state (else-stmt stmt)  ; no nested if statements
                                                   (m-state (cond-stmt stmt) state))))))

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
       (parse-while stmt (m-state (statement stmt) (m-state (condition stmt) state))))
      (else (m-state (condition stmt) state))))) ; condition is false; stop loop

; abstraction for while statement
(define condition cadr)
(define statement caddr)

; parse-return parses a return statement
(define parse-return
  (lambda (stmt state)
    (parse-asgn 'return (return-val stmt) (addbinding 'return 'novalue state))))

; Helper function for parse-return; return the expression or value to be returned
(define return-val (lambda (stmt) (car (cdr stmt))))