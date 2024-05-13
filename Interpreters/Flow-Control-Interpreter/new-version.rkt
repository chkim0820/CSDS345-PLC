#lang racket
(require "simpleParser.rkt")
(require test-engine/racket-tests)

; Authors: Chae Kim, Amir Aliyu
; Date: Mar 16, 2024
; Description: Flow Control Interpreter

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

;============================================================================
; MAIN M-STATE FUNCTION
;============================================================================

; m-state changes the state based on certain statements
(define m-state
  (lambda (stmt state next return continue break throw)
    (cond
      ((not (list? stmt))               (next state))
      ((eq? (operator stmt) '=)         (parse-asgn (arg1 stmt) (arg2 stmt) state next))
      ((eq? (operator stmt) 'var)       (parse-decl stmt state next))
      ((eq? (operator stmt) 'while)     (parse-while stmt state next return (lambda (v) v) (lambda (v) v) throw))
      ((eq? (operator stmt) 'return)    (return (parse-return stmt state)))
      ((eq? (operator stmt) 'if)        (parse-if stmt state next return continue break throw))
      ((eq? (operator stmt) 'begin)     (parse-block stmt state next return continue break throw)) ; For a block of code
      ((eq? (operator stmt) 'continue)  (continue (rmv-layer state)))
      ((eq? (operator stmt) 'break)     (break (rmv-layer state)))
      ((eq? (operator stmt) 'try)       (parse-try stmt state next return continue break throw))
      ((eq? (operator stmt) 'throw)     (throw (arg1 stmt) (rmv-layer state)))
      ((not (roperand? stmt))           (m-state (loperand stmt) state next return continue break throw)) ; no right operand
      (else (m-state (loperand stmt) state
                     (lambda (v1) (m-state (roperand stmt) (next v1) (lambda (v2) v2) return continue break throw)) return continue break throw))))) ; Else, it's m-int or m-bool with args to update


;============================================================================
; STATEMENT EVALUATION FUNCTIONS (i.e.parse-asgn, parse-decl, etc.)
;============================================================================

; parse-asgn takes in two statements and a state and adds the binding of the new value to the state 
(define parse-asgn 
  (lambda (var expr state next)
    (cond
      ((equal? (lookup-layers var state) 'not-initialized) (error "Variable not declared yet")) ; var not declared yet
      ((number? expr)                      (next (updatebinding-layers var expr state))) ; assigning int
      ((boolean? expr)                     (next (updatebinding-layers var (bool-cvt expr) state))) ; assigning bool
      ((not (list? expr))                  (next (updatebinding-layers var (lookup-layers expr state) state))) ; variable
      ((bool-check (operator expr))        (next (updatebinding-layers var (make-boolean expr state) (updated-state expr state))))                                             
      ((int-check (operator expr))         (next (updatebinding-layers var (m-int expr state) (updated-state expr state))))                                              
      (else                                (next (updatebinding-layers var ; handle assignment within assignment
                                                       (lookup-layers (arg1 expr) (parse-asgn (arg1 expr) (arg2 expr) state))
                                                       (parse-asgn (arg1 expr) (arg2 expr) state)))))))

; parse-decl takes a declaration statement and adds it to the state with atom 'novalue to show it hasnt been given a value
(define parse-decl
  (lambda (stmt state next)
    (cond
      ((not (exp-arg stmt))                (next (addbinding-layers (arg1 stmt) 'novalue state))) ; no assignment
      ((has-binding (arg1 stmt) state) (error "redefining")) ; if the argument is already in the state, throw a redefining error
      (else                                (parse-asgn (arg1 stmt) (arg2 stmt) (addbinding-layers (arg1 stmt) 'novalue state) next)))))


; parse-if evaluates if statements
(define parse-if
  (lambda (stmt state next return continue break throw)
    (cond
      ((m-bool (cond-stmt stmt) state)     (true-condition stmt state next return continue break throw))
      ((equal? 'no-else (else-stmt stmt))  (no-else-stmt stmt state next return continue break throw)) ; no else
      ((nested-if (else-stmt stmt))        (nested-if-stmt stmt state next return continue break throw)) ; nested if statements
      (else                                (false-condition stmt state next return continue break throw)))))  ; no nested if statements

; parse-while evaluates while statements
(define parse-while
  (lambda (stmt state next return continue break throw)
    (cond
      ((m-bool (condition stmt) state)     (loop stmt state next return continue break throw)) ; condition is true; continue looping 
      (else                                (exit-loop stmt state next return continue break throw))))) ; condition is false; stop loop

; parse-return evaluates return statements
(define parse-return
  (lambda (stmt state) 
    (cond
      ((num-operation? stmt state)         (m-int (return-val stmt) state)) ; num operation
      ((bool-operation? stmt state)        (make-boolean (return-val stmt) state)) ; bool operation
      ((number? (return-val stmt))         (return-val stmt))
      ((boolean? (return-val stmt))        (bool-cvt (return-val stmt)))
      (else                                (lookup-layers (return-val stmt) state)))))

; parse-block evaluates code blocks
(define parse-block
  (lambda (stmt layers next return continue break throw) ; assume layers are inputted
    (cond
      ((empty-stmt stmt)                            (next (rmv-layer layers))) ; remove the current layer at the end of block
      ((eq? 'begin (curr-stmt stmt))                (parse-block (next-stmts stmt) (add-layer layers) next return continue break throw))
      ((eq? 'continue (keyword (curr-stmt stmt)))   (continue (rmv-layer layers)))
      ((eq? 'break (keyword (curr-stmt stmt)))      (break (rmv-layer layers)))
      (else                                         (next-block stmt layers next return continue break throw))))) ; parse the next block

; parse-try evaluates try/catch/finally blocks
(define parse-try
  (lambda (stmt state next return continue break throw)
    (if (null? stmt)
        '()
        (try-catch-finally stmt state next return continue break throw))))


;============================================================================
; INT/BOOLEAN EVALUATION FUNCTIONS 
;============================================================================

; m-bool takes a syntax rule and a state and produces a true / false value (or an error condition).
(define m-bool 
  (lambda (boolexp state)
    (cond
      ((number? boolexp)                 boolexp)
      ((symbol? boolexp)                 (return-boolean boolexp state))
      ((intexp? boolexp)                 (m-int boolexp state)) ; handle intexps nested in boolean expressions
      ((has-assignment boolexp state)    (handle-boolean-side-effects boolexp state)) ; if an assignment is nested in a boolexp 
      ((eq? (operator boolexp) '&&)      (and         (loperand-boolean boolexp state)    (right-boolexp boolexp state)))
      ((eq? (operator boolexp) '||)      (or          (loperand-boolean boolexp state)    (right-boolexp boolexp state)))
      ((eq? (operator boolexp) '!)       (not                                             (left-boolexp boolexp state)))
      ((eq? (operator boolexp) '==)      (equal?      (loperand-boolean boolexp state)    (right-boolexp boolexp state)))
      ((eq? (operator boolexp) '!=)      (not (equal? (loperand-boolean boolexp state)    (right-boolexp boolexp state))))
      ((eq? (operator boolexp) '<)       (<           (loperand-boolean boolexp state)    (right-boolexp boolexp state)))
      ((eq? (operator boolexp) '>)       (>           (loperand-boolean boolexp state)    (right-boolexp boolexp state)))
      ((eq? (operator boolexp) '<=)      (<=          (loperand-boolean boolexp state)    (right-boolexp boolexp state)))
      ((eq? (operator boolexp) '>=)      (>=          (loperand-boolean boolexp state)    (right-boolexp boolexp state))))))

; m-int takes in an expression and a state and returns a numeric value
(define m-int 
  (lambda (intexp state)
    (cond
      ((number? intexp)                  intexp)
      ((symbol? intexp)                  (return-integer intexp state)) ; lookup variable value
      ((has-assignment intexp state)     (handle-int-side-effects intexp state)) ; if an assignment is nested in an intexp
      ((and (eq? (operator intexp) '-)   (unary? intexp)) (* '-1 (m-int (loperand intexp) (left-intexp intexp state))))
      ((eq? (operator intexp) '+)        (+ (loperand-intexp intexp state)                (right-intexp intexp state)))
      ((eq? (operator intexp) '-)        (- (loperand-intexp intexp state)                (right-intexp intexp state)))
      ((eq? (operator intexp) '*)        (* (loperand-intexp intexp state)                (right-intexp intexp state)))
      ((eq? (operator intexp) '/)        (quotient (loperand-intexp intexp state)         (right-intexp intexp state)))
      ((eq? (operator intexp) '%)        (remainder (loperand-intexp intexp state)        (right-intexp intexp state))))))

;============================================================================
; STATE FUNCTIONS (i.e. lookup, addbinding, etc.)
;============================================================================

; traverse all layers in order and return/update first match found
(define lookup-layers
  (lambda (var layers)
    (cond
      ((empty-layers layers)                      (error "using before declaring"))
      ((empty-state (curr-layer layers))          (lookup-layers var (next-layers layers))) ; current state is empty
      ((equal? (get-var (curr-layer layers)) var) (get-val (curr-layer layers))) ; current layer has a variable that matches the input
      (else                                       (lookup-layers var (next-curr-layer layers)))))) ; continue in current layer

; add a binding if it does not already exist; if a binding exists, update it
(define addbinding-layers
  (lambda (var val layers)
    (cond
      ((empty-layers layers) layers) ; state is empty; return original state
      ((false? (has-binding var layers)) ; var not in the layers yet; add to the top layer
       (cons (addbinding var val (curr-layer layers)) (rmv-layer layers)))
      (else (cons (curr-layer layers) (updatebinding-layers var val (next-layers layers))))))) ; update existing binding if exists

; remove a binding from the state while accounting for multiple layers
(define removebinding-layers
  (lambda (var layers)
    (cond
      ((empty-layers layers) layers) ; state is empty; return original state
      ((false? (find-var var (curr-layer layers))) (cons (curr-layer layers) (removebinding-layers var (next-layers layers))))
      (else (cons (removebinding var (curr-layer layers)) (rmv-layer layers))))))

; update a binding while accounting for layers
(define updatebinding-layers
  (lambda (var val layers)
    (cond
      ((empty-layers layers) layers) ; state is empty; return original state
      ((false? (find-var var (curr-layer layers))) (cons (curr-layer layers) (updatebinding-layers var val (next-layers layers))))
      (else (cons (updatebinding var val (curr-layer layers)) (rmv-layer layers))))))

;============================================================================
; STATEMENT FUNCTION HELPERS (i.e.parse-asgn, parse-decl, etc.)
;============================================================================

; ******************** PARSE-ASGN HELPERS *************************

; make-boolean converts #t/#f into 'true/'false respectively
(define make-boolean
  (lambda (expr state)
    (bool-cvt (m-bool expr state))))

; returns the state that the binding should update based on
(define updated-state
      (lambda (expr state)
        (m-state expr state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))))

(define asgn-var (lambda (stmt) (if (not (list? stmt)) stmt (arg1 stmt)))) ; returns the var of asgn
(define new-stmt (lambda (oper left right) (list oper left right))) ; new statement w/o assignments
(define is-asgn (lambda (stmt) (if (and (list? stmt) (eq? (operator stmt) '=)) #t #f))) ; returns whether stmt is assignment or not

; ******************** PARSE-DECL HELPERS **************************

; exp-arg returns true if assignment with declaration, else false
(define exp-arg (lambda (stmt) (if (null? (decl-exp stmt)) #f #t)))

; For converting boolean (#t/f) to true or false
(define bool-cvt (lambda (expr) (cond ((eq? #t expr) 'true) ((eq? #f expr) 'false) (expr))))

; abstraction for exp-arg helper
(define decl-exp
  (lambda (stmt)
    (cdr (cdr stmt))))

; ******************** PARSE-IF HELPERS ****************************

; defines continuation if the condition is true
(define true-condition
  (lambda (stmt state next return continue break throw)
    (m-state (cond-stmt stmt) state (lambda (v) (m-state (then-stmt stmt) v (lambda (v1) (next v1)) return continue break throw)) return continue break throw)))

; defines continuation if condition is false
(define false-condition
  (lambda (stmt state next return continue break throw)
    (m-state (cond-stmt stmt) state (lambda (v) (m-state (else-stmt stmt) v (lambda (v1) (next v1)) return continue break throw)) return continue break throw)))

; defines continuation if there is no else
(define no-else-stmt
  (lambda (stmt state next return continue break throw)
    (m-state (cond-stmt stmt) state (lambda (v) (next v)) return continue break throw)))

; defines continuation for a nested if-statement
(define nested-if-stmt
  (lambda (stmt state next return continue break throw)
    (m-state (cond-stmt stmt) state (lambda (v) (parse-if (else-stmt stmt) v (lambda (v1) (next v1)) return continue break throw)) return continue break throw)))

; abstract different parts of conditional
(define cond-stmt (lambda (stmt) (car (cdr stmt)))); conditional statement
(define then-stmt (lambda (stmt) (car (cdr (cdr stmt))))) ; then statement
(define else-stmt (lambda (stmt) ; optional else statement
                    (if (null? (cdr (cdr (cdr stmt)))) 'no-else
                        (car (cdr (cdr (cdr stmt)))))))

; return whether there's a nested conditional
(define nested-if
  (lambda (else-stmt)
    (cond
      ((null? else-stmt) 'no-else)
      ((equal? 'if (car else-stmt)) #t)
      (else #f))))

; ******************** PARSE-WHILE HELPERS *************************

; helper that continues looping
(define loop
  (lambda (stmt state next return continue break throw)
    (m-state (condition stmt) state
                (lambda (new-state)
                  (define new-next (lambda (nxt) (m-state stmt nxt (lambda (v) (next v)) return (lambda (v) v) (lambda (v) v) throw)))
                  (define new-cont (lambda (cont) (m-state stmt cont (lambda (v) (next v)) return (lambda (v) v) (lambda (v) v) throw)))
                  (define new-break (lambda (br) (next br)))
                  (m-state (body stmt) new-state new-next return new-cont new-break throw))
                return continue break throw)))

; helper that exits the loop and evaluates the next statement
(define exit-loop
  (lambda (stmt state next return continue break throw)
    (m-state (condition stmt) state (lambda (v) (next v)) return continue break throw)))

; abstract different parts of while statement
(define condition cadr)
(define body caddr)

; ******************** PARSE-RETURN HELPERS ************************

; returns whether the return value is a numerical operation
(define num-operation?
  (lambda (stmt state)
    (and (list? (return-val stmt)) (int-check (operator (return-val stmt))))))

; returns whether the return value is a boolean operation
(define bool-operation?
  (lambda (stmt state)
    (and (list? (return-val stmt)) (bool-check (operator (return-val stmt))))))

; abstract different parts of return
(define return-val (lambda (stmt) (car (cdr stmt))))

; ******************** PARSE-BLOCK HELPERS *************************

; helper that exits the block and parse the next one
(define next-block
  (lambda (stmt layers next return continue break throw)
    (m-state (curr-stmt stmt) layers (lambda (state) (parse-block (next-stmts stmt) state next return continue break throw)) return continue break throw)))

; abstract different parts of block
(define curr-stmt (lambda (stmt) (if (null? stmt) stmt (car stmt))))
(define empty-stmt (lambda (stmt) (if (null? stmt) #t #f)))
(define next-stmts (lambda (stmt) (if (null? stmt) stmt (cdr stmt))))
(define keyword (lambda (stmt) (if (null? stmt) stmt (car stmt))))
; Some also used for parse-try

; ******************** PARSE-TRY HELPERS ***************************

; helper that implements try-catch-finally blocks
(define try-catch-finally
  (lambda (stmt state next return continue break throw)
    (define try-block (cons 'begin (curr-stmt (next-stmts stmt)))) ; i.e. ((= x 20) (if (< x 0) (throw 10)) (= x (+ x 5)))
       (define new-next (lambda (v) (m-state (finally-block stmt) v next return continue break throw))) ; finally
       (define new-throw (lambda (e st) (parse-block (catch-block stmt) (catch-state stmt e st)
                                                     new-next return continue break throw)))
       (m-state try-block state new-next return continue break new-throw))) ; beginning of try/catch/finally

; helper that adds binding for the specified error name
(define catch-state
  (lambda (stmt val state)
    (cond
      ((null? (catch-block stmt)) state) ; there's no catch block; no change
      (else (addbinding-layers (catch-var stmt) val (add-layer state))))))

; abstract different part of try-catch-finally vlocks
(define catch-var (lambda (stmt) (caadr (caddr stmt))))

; abstract catch block
(define catch-block
  (lambda (stmt)
    (define 2nd-block (car (cdr (cdr stmt))))
    (cond
      ((null? (cdr (cdr stmt))) '())
      ((eq? 'catch (keyword 2nd-block)) (car (cddr 2nd-block))))))

; abstract finally block
(define finally-block
  (lambda (stmt)
    (define 2nd-block (car (cdr (cdr stmt))))
    (define 3rd-block (car (cdr (cdr (cdr stmt)))))
    (cond
      ((null? (cdr (cdr stmt))) '())
      ((eq? 'finally (keyword 2nd-block)) (cons 'begin (cadr 2nd-block)))
      ((null? (cdr (cdr (cdr stmt)))) '())
      ((eq? 'finally (keyword 3rd-block)) (cons 'begin (cadr 3rd-block))))))

;============================================================================
; M-INT / M-BOOL HELPERS 
;============================================================================

; ******************** M-BOOL HELPERS *************************

; helper that handles assignment statements within booleans
(define handle-boolean-side-effects
  (lambda (boolexp state)
    (m-bool (new-stmt (operator boolexp) (value-get (asgn-var (loperand boolexp)) (m-state (loperand boolexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))
                        (value-get (asgn-var (roperand boolexp)) (m-state (roperand boolexp) (left-boolexp boolexp state) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))))
               (m-state (roperand boolexp) (left-boolexp boolexp state) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))

; return whether this is operator is a boolean operator  
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

; return whether an expression is a boolean expression
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

; return the value of the boolean symbol or a using before assigning error
(define return-boolean
  (lambda (boolexp state)
    (if (equal? (lookup-layers boolexp state) 'novalue)
                                        (error "using before assigning")
                                        (lookup-layers boolexp state))))

; returns whether or not the left operand of a boolexp is true
(define loperand-boolean
  (lambda (boolexp state)
    (m-bool (loperand boolexp) state)))

; returns a continuation to evaluate the right part of the boolean expression
(define right-boolexp
  (lambda (boolexp state)
    (m-bool (roperand boolexp) (m-state (loperand boolexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))
                  
; returns a continuation to evaluate the left part of a boolean expression
(define left-boolexp
  (lambda (boolexp state)
    (m-bool (loperand boolexp) (m-state (loperand boolexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))

; returns whether theres an assignment statement in either the left/right operand of the boolean expression
(define has-assignment
  (lambda (boolexp state)
    (or (is-asgn (loperand boolexp)) ; handle assignment statements within booleans
           (and (roperand? boolexp) (is-asgn (roperand boolexp))))))

; ******************** M-INT HELPERS *************************

; helper that handles assignment statements within integers
(define handle-int-side-effects
  (lambda (intexp state)
    (m-int (new-stmt (operator intexp) (value-get (asgn-var (loperand intexp)) (m-state (loperand intexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))
                        (value-get (asgn-var (roperand intexp)) (m-state (roperand intexp) (m-state (loperand intexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))) state)))

; returns whether this operator is an integer operator
(define int-check
  (lambda (oper)
    (cond
      ((equal? oper '+) #t)
      ((equal? oper '-) #t)
      ((equal? oper '*) #t)
      ((equal? oper '/) #t)
      ((equal? oper '%) #t)
      (else #f))))

; returns whether an expression is an integer expression, i.e. does it start with an arithmetic operator?
(define intexp?
  (lambda (exp)
    (or (eq? (operator exp) '+)
        (eq? (operator exp) '-)
        (eq? (operator exp) '*)
        (eq? (operator exp) '/)
        (eq? (operator exp) '%))))

; returns the value of the integer or a using before assigning error
(define return-integer
  (lambda (intexp state)
    (if (equal? (lookup-layers intexp state) 'novalue)
                                      (error "using before assigning")
                                      (lookup-layers intexp state)))) ; lookup variable value

; returns the left operand of the int expression
(define loperand-intexp
  (lambda (intexp state)
    (m-int (loperand intexp) state)))

; returns a continuation to evaluate the right integer operand
(define right-intexp
  (lambda (intexp state)
    (m-int (roperand intexp) (m-state (loperand intexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))
                   

; returns a continuation to evaluate the right integer operand 
(define left-intexp
  (lambda (intexp state)
    (m-int (loperand intexp) (m-state (loperand intexp) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))
    
; returns whether the expression is unary or not
(define unary? (lambda (exp) (if (null? (cdr (cdr exp))) #t #f)))
(define value-get (lambda (stmt state) (if (or (number? stmt) (boolean? stmt)) stmt (lookup-layers stmt state)))) ; return value of input

;============================================================================
; STATE FUNCTION HELPERS 
;============================================================================

; return whether a variable already exists in layers/states
(define has-binding
  (lambda (var layers)
    (cond
      ((empty-layers layers) #f)
      ((empty-state (curr-layer layers)) (has-binding var (next-layers layers)))
      ((equal? (get-var (curr-layer layers)) var) #t)
      (else (has-binding var (next-curr-layer layers))))))

; return whether var already exists in current state/layer
(define find-var
  (lambda (var state)
    (cond
      ((empty-state state) #f)
      ((equal? (get-var state) var) #t)
      (else (find-var var (next-state state))))))

; updatebinding adds a binding if it does not already exist; if a binding exists, it updates.
(define updatebinding
  (lambda (var val state)
    (addbinding var val (removebinding var state)))) ;remove existing bindings first & add new binding

; removebinding removes a binding from the state
(define removebinding
  (lambda (var state)
    (cond
      ((empty-state state)          state) ; state is empty; return original state
      ((equal? (get-var state) var) (next-state state)) ; found var; remove binding
      (else                         (addbinding (get-var state) ; move onto the next
                                                (get-val state)
                                                (removebinding var (next-state state)))))))

; addbinding adds a binding to the state
(define addbinding
  (lambda (var val state)
    (list (cons var (get-vars state)) (cons val (get-vals state))))) ; adding to state

; lookup function; return value of var or if it is not declared yet (error) or novalue (for single-layered states in pt. 1)
(define lookup
  (lambda (var state)
    (cond
      ((empty-state state)          (error "using before declaring")) ; var not initialized yet ; FIX?
      ((empty-state state)          state)
      ((equal? (get-var state) var) (get-val state)) ; returns value or 'novalue depending on assignment status
      (else                         (lookup var (next-state state)))))) ; not equal; recurse down further


;============================================================================
; LAYER ABSTRACTIONS 
;============================================================================

; Helper functions for abstraction
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
(define add-layer (lambda (layers) (cons '(() ()) layers)))
(define rmv-layer (lambda (layers) (cdr layers)))


;============================================================================
; SIMPLE ABSTRACTIONS 
;============================================================================

(define roperand? (lambda (stmt) (if (null? (cddr stmt)) #f #t))) ; true if no 2nd operand

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

;============================================================================
; PROGRAM ABSTRACTIONS 
;============================================================================

; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
(define parse-prog
  (lambda (prog state)
    (cond
      ((null? prog) (error "no return statement"))
      (else (m-state (curr-line prog) state
                     (lambda (new-state) (parse-prog (next-lines prog) new-state))
                     (lambda (v) v)
                     (lambda (v) (error "invalid continue"))
                     (lambda (v) (error "invalid break"))
                     (lambda (v1 v2) (error "error thrown"))))))) ; Parse the next statement

; helper function for next program
(define curr-line (lambda (prog) (if (null? prog) prog (car prog))))
(define next-lines (lambda (prog) (if (null? prog) prog (cdr prog))))

;============================================================================
; TEST CASES
;============================================================================

; test cases for part 1
(check-expect (interpret "MakeTestsPart1/test1a.txt")   150)
(check-expect (interpret "MakeTestsPart1/test2a.txt")   -4)
(check-expect (interpret "MakeTestsPart1/test3..txt")   10)
(check-expect (interpret "MakeTestsPart1/test4a.txt")   16)
(check-expect (interpret "MakeTestsPart1/test5a.txt")   220)
(check-expect (interpret "MakeTestsPart1/test6a.txt")   5)
(check-expect (interpret "MakeTestsPart1/test7a.txt")   6)
(check-expect (interpret "MakeTestsPart1/test8a.txt")   10)
(check-expect (interpret "MakeTestsPart1/test9a.txt")   5)
(check-expect (interpret "MakeTestsPart1/test10a.txt")  -39)
(check-error  (interpret "MakeTestsPart1/test11a.txt")  "using before declaring")
(check-error  (interpret "MakeTestsPart1/test12a.txt")  "using before declaring")
(check-error  (interpret "MakeTestsPart1/test13a.txt")  "using before assigning")
(check-error  (interpret "MakeTestsPart1/test14a.txt")  "redefining")
(check-expect (interpret "MakeTestsPart1/test15a.txt")  'true)
(check-expect (interpret "MakeTestsPart1/test16a.txt")  100)
(check-expect (interpret "MakeTestsPart1/test17a.txt")  'false)
(check-expect (interpret "MakeTestsPart1/test18a.txt")  'true)
(check-expect (interpret "MakeTestsPart1/test19a.txt")  128)
(check-expect (interpret "MakeTestsPart1/test20a.txt")  12)

; test cases for part 2
(check-expect (interpret "MakeTestsPart2/test1b.txt")   20)
(check-expect (interpret "MakeTestsPart2/test2b.txt")   164)
(check-expect (interpret "MakeTestsPart2/test3b.txt")   32)
(check-expect (interpret "MakeTestsPart2/test4b.txt")   2)
(check-error  (interpret "MakeTestsPart2/test5b.txt")   "using before declaring")
(check-expect (interpret "MakeTestsPart2/test6b.txt")   25)
(check-expect (interpret "MakeTestsPart2/test7b.txt")   21)
(check-expect (interpret "MakeTestsPart2/test8b.txt")   6)
(check-expect (interpret "MakeTestsPart2/test9b.txt")   -1)
(check-expect (interpret "MakeTestsPart2/test10b.txt")  789)
(check-error  (interpret "MakeTestsPart2/test11b.txt")  "using before declaring")
(check-error  (interpret "MakeTestsPart2/test12b.txt")  "using before declaring")
(check-error  (interpret "MakeTestsPart2/test13b.txt")  "invalid break")
(check-expect (interpret "MakeTestsPart2/test14b.txt")  12)
(check-expect (interpret "MakeTestsPart2/test15b.txt")  125)
(check-expect (interpret "MakeTestsPart2/test16b.txt")  110)
(check-expect (interpret "MakeTestsPart2/test17b.txt")  2000400)
(check-expect (interpret "MakeTestsPart2/test18b.txt")  101)
(check-error  (interpret "MakeTestsPart2/test19b.txt")  "error thrown")
(check-expect (interpret "MakeTestsPart2/test20b.txt")  21)

