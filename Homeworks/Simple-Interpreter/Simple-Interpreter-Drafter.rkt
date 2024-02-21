#lang racket
(require "simpleParser.rkt")

(define main
  (lambda (filename)
    (parse-prog (parser filename) '(()())))) ; the list is the state & 1st is syntax tree

; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
(define parse-prog ; parsing the program
  (lambda (prog state) ; syntax tree & state
    (if (null? prog) ; empty syntax tree
        (lookup 'return state) ; End of program. Give the return value. ; COME BACK
        (parse-prog (cdr prog) (m-state (car prog) state)) )))

; TODO FINISH THE FUNCTION ABOVE (I'M NOT SURE THIS WORKS).

;============================================================================
; STATE FUNCTIONS (i.e. lookup, addbinding, etc.)
;============================================================================

; The state is a list with two sublists; ((declared vars) (values))

; helpers to get the vars and values of the state; returns all vars and vals
(define get-vars (lambda (state) (car state))) ; all vars
(define get-vals (lambda (state) (cadr state))) ; all vals
(define next-state (lambda (state) (if (empty-state state) state (list (cdr (car state)) (cdr (cadr state)))))) ; iterate to next states
(define empty-state (lambda (state) (if (null? (car state)) #T #F))); assume it works (num. variables > values)
(define get-var (lambda (state) (car (get-vars state)))) ; get first var; assume null state checked beforehand
(define get-val (lambda (state) (car (get-vals state)))) ; get first val

; lookup function; return value of var or if it is not declared yet (error) or novalue
(define lookup
  (lambda (var state) 
    (cond
      ((empty-state state) 'not-declared-yet) ; var not declared yet; no more var
      ((equal? (get-var state) var) (get-val state)) ; returns value or 'novalue depending on assignment status
      ((lookup var (next-state state)))))) ; not equal; recurse down further

; addbinding function
(define addbinding
  (lambda (var val state)
    (list (cons var (get-vars state)) (cons val (get-vals state)))))

; removebinding function
(define removebinding ; FIX; doesn't work for some reason
  (lambda (var state)
    (cond
      ((empty-state state) state) ; state is empty; return original state
      ((equal? (get-var state) var) (next-state state)) ; found var; remove binding
      ((addbinding (get-var state) (get-val state) (removebinding var (next-state state))))))) ; move onto the next


; updatebinding function
; updatebinding adds a binding if it does not already exist.
; FIX? how about it already exists?
(define updatebinding
  (lambda (var val state)
    (addbinding var val (removebinding var state)) ))

;============================================================================
; EVAL FUNCTIONS (i.e. M_state, M_int, M_bool, etc.)
;============================================================================

; Functions to define the parts of an expression, demonstrating two ways to write them.
(define operator car)
(define loperand cadr)
(define roperand caddr)

(define arg1 cadr)
(define arg2 caddr)
(define arg3 cadddr)

; state should call parse-decl, parse-asgn, etc based on which operation is necessary (i think?)
; and then return the changed state
; Also need to implement: 
; looking up a binding, creating a new binding, and updating an existing binding

; proposing to change 'state' to be called M_state, assuming below functions to be 
; for changing state 
(define m-state
  (lambda (stmt state)
    (cond
      ((eq? (operator stmt) '=)      (parse-asgn (arg1 stmt) (arg2 stmt) state))
      ((eq? (operator stmt) 'var)    (parse-decl (loperand stmt) (roperand stmt) state))
      ((eq? (operator stmt) 'while)  (parse-while (loperand stmt) (roperand stmt) state))
      ((eq? (operator stmt) 'return) (parse-return (loperand stmt) (roperand stmt) state))
      ((eq? (operator stmt) 'if)     (parse-if (loperand stmt) (roperand stmt) state)) )))


; Parse an assignment statement.
; takes in two statements and a state and adds the binding of the new value to the state 
(define parse-asgn
  (lambda (stmt state)
    (cond
      ((number? (roperand stmt)) (addbinding (loperand stmt) (roperand stmt) (removebinding (loperand stmt) state))) ; if the right operand is a number, add binding
      (else (addbinding (loperand stmt) (lookup (roperand stmt) state) (removebinding (loperand stmt) state)))))) ; if the right operand is a variable, use its value


; parse-decl should take a declaration statement and add it to the state with atom 'novalue to show it hasnt been given a value
(define parse-decl
  (lambda (stmt state)
    (cond
      ; if it is in the format (var x value)
      ((not (null? (decl-value stmt))) (addbinding (loperand stmt) (roperand stmt) state))
      ; if it is in the format (var x)
      ((eq? (lookup (loperand stmt) state) 'novalue) (addbinding (loperand stmt) 'novalue state))
      (else state)))) ; if the variable has already been declared/assigned, do nothing

; abstraction to help check which format the declaration is in
(define decl-value
  (lambda (stmt)
    (cdr (cdr stmt))))

; TODO: FINISH THESE STATEMENT FUNCTIONS.
(define parse-while ; ex) (while (!= (% y x) 3) (= y (+ y 1)))
  (lambda (stmt state)
    '()))
(define parse-return '())
(define parse-if     '())

; note - we probably need the following mappings/ something like it:
; M_int takes a syntax rule and a state and produces a numeric value (or an error condition).
; M_boolean takes a syntax rule and a state and produces a true / false value (or an error condition).
; M_name takes a syntax rule and produces a name (or an error condition).


; M_boolean takes a syntax rule and a state and produces a true / false value (or an error condition).
; this implementation of M_boolean returns #t or #f
; M_boolean takes a syntax rule and a state and produces a true / false value (or an error condition).
(define M_boolean
  (lambda (stmt state)
    (cond
      ; if the stmt is evaluating &&, ||, or ! 
      ((eq? (bOperator stmt) '&&) (and (M_boolean (leftExp stmt) state) (M_boolean (rightExp stmt) state)))
      ((eq? (bOperator stmt) '||) (or (M_boolean (leftExp stmt) state) (M_boolean (rightExp stmt) state)))
      ((eq? (bOperator stmt) '!) (not (M_boolean (leftExp stmt) state) (M_boolean (rightExp stmt) state)))
      ; if the operator is ==, (implemented to test &&, ||, and !)
      ((eq? (bOperator stmt) '==) (eq? (leftCompared stmt) (rightCompared stmt)))
      ((eq? (bOperator stmt) '<) (< (leftCompared stmt) (rightCompared stmt)))
      ((eq? (bOperator stmt) '>) (> (leftCompared stmt) (rightCompared stmt)))
      ((eq? (bOperator stmt) '<=) (<= (leftCompared stmt) (rightCompared stmt)))
      ((eq? (bOperator stmt) '>=) (>= (leftCompared stmt) (rightCompared stmt))))))


; abstraction for M_boolean:
(define bOperator
  (lambda (lis)
    (car lis)))
(define leftExp
  (lambda (lis)
    (car (cdr lis))))
(define rightExp
  (lambda (lis)
    (car (cdr (cdr lis)))))
(define leftCompared
  (lambda (lis)
    (car (cdr lis))))
(define rightCompared
  (lambda (lis)
    (car (cdr (cdr lis)))))

; M_int takes in an expression and a state and returns a numeric value
(define M_int
  (lambda (intexp state)
    (cond
      ((number? intexp) intexp) ; modify state? or does it look up values of variables FIX
      ((eq? (operator intexp) '+) (+ (M_int (loperand intexp) state) (M_int (roperand intexp) state)))
      ((eq? (operator intexp) '-) (- (M_int (loperand intexp) state) (M_int (roperand intexp) state)))
      ((eq? (operator intexp) '*) (* (M_int (loperand intexp) state) (M_int (roperand intexp) state)))
      ((eq? (operator intexp) '/) (quotient (M_int (loperand intexp) state) (M_int (roperand intexp) state)))
      ((eq? (operator intexp) '%) (remainder (M_int (loperand intexp) state) (M_int (roperand intexp) state))))))
