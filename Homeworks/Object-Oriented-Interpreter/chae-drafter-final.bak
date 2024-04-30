#lang racket
(require "functionParser.rkt")
(require test-engine/racket-tests)

; Authors: Chae Kim, Amir Aliyu
; Date: April 4, 2024
; Description: Flow Control Interpreter

;============================================================================
; INTERPRET (MAIN) FUNCTION
;============================================================================

; interpret takes a filename, calls parser with the filename, evaluates the syntax tree
; returned by parser, and returns the proper value.
(define interpret
  (lambda (filename)
    (parse-prog (parser filename) '[((true false) (#t #f))]))) ; the layer contains the initial state (expressed with [])
                                                                              ; the initial state is a list with 2 sublists
                                                                              ; true or false stored as default

;============================================================================
; PROGRAM PARSER 
;============================================================================

; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
(define parse-prog
  (lambda (prog state)
    (cond
      ((null? prog) (m-value '(funcall main) (add-layer state) ; run the main function
                             (lambda (v) v) ; initializing next
                             (lambda (v) v)
                             (lambda (v) (error "invalid continue"))
                             (lambda (v) (error "invalid break"))
                             (lambda (v1 v2) (error "error thrown"))))
      (else         (outer-layer (curr-line prog) state ; handle the outer layer
                                 (lambda (new-state) (parse-prog (next-lines prog) new-state))
                                 (lambda (v) v)
                                 (lambda (v) (error "invalid continue"))
                                 (lambda (v) (error "invalid break"))
                                 (lambda (v1 v2) (error "error thrown"))))))) ; Parse the next statement

; helper function for next program
(define curr-line (lambda (prog) (if (null? prog) prog (car prog))))
(define next-lines (lambda (prog) (if (null? prog) prog (cdr prog))))


;============================================================================
; FUNCTION PROCESSING; added for project 3!!!
;============================================================================
; Function call syntax: '(function name formal-params body)

; outer-layer does m-state functions for variable declarations and function definitions
(define outer-layer
  (lambda (stmt state next return continue break throw)
    (cond
      ((not (list? stmt))               (next state)) ; at the end; will call main-method
      ((eq? (operator stmt) 'var)       (parse-decl stmt state next return continue break throw))
      ((eq? (operator stmt) 'function)  (parse-func stmt state next return continue break throw))
      (else                             (error "Only function calls and assignments allowed on the base layer"))))) ; Else, it's m-int or m-bool with args to update

; m-value for function calls (funcalls)
(define m-value
  (lambda (funcall state next return continue break throw)
    (define closure (lookup-layers (func-name funcall) state)) ; defining the function's closure (param body func)
    (m-state (closure-body closure) (environment closure (arguments funcall) state return) ; call the function in m-state
             next return continue break throw)))

; returns static scoping instead of dynamic scoping
(define static-scoping
  (lambda (name old-state) ; old state is the state at the time of base layer creation
    (lambda (closure actual-params new-state return)
      (environment-func name closure return actual-params old-state new-state 0))))

; calling an environment function with an input: (environment-func state)
(define environment
  (lambda (closure actual-params state return)
    ((caddr closure) closure actual-params state return)))

; returns the environment function of a function in its closure
; assuming the formal parameters are different
; FIX; make tail-recursive
(define environment-func
  (lambda (name closure return actual-params old-state new-state ind) ; ind returns the index of
    (cond
      ((and (eq? ind 0) (not (eq? (length (formal-params closure)) (length actual-params))))
       (error "mismatched parameters and arguments"))
      ;; For initializing new state for running a function with static scoping, add the current layer of the outer function.
      ((null? actual-params) (add-layer (parse-decl (list 'var name closure) ; add current function's closure
                                                    (update-global-vars old-state new-state return ; should not disturb the original order
                                                                        (base-layer old-state (length old-state))
                                                                        (base-layer new-state (length new-state)))
                                          (lambda(v)v) (lambda(v)v) (lambda(v)v) (lambda(v)v) (lambda(v)v))))
      ;; For binding actual and formal parameter, use the state of the function current function is called from
      ((num? (curr-actual actual-params)) ; number or numerical expression as param input
       (addbinding-layers (curr-formal (formal-params closure) ind)
                          (m-int (curr-actual actual-params) new-state
                                 (lambda(v)v) (lambda(v)v) (lambda(v)v) (lambda(v)v) (lambda(v)v))
                          (environment-func name closure return (next-params actual-params) old-state new-state (+ ind 1))))      
      ((bool? (curr-actual actual-params)) ; boolean or boolean expression as param input
       (addbinding-layers (curr-formal (formal-params closure) ind)
                          (m-bool (curr-actual actual-params) new-state
                                  (lambda(v)v) (lambda(v)v) (lambda(v)v) (lambda(v)v) (lambda(v)v))
                          (environment-func name closure return (next-params actual-params) old-state new-state (+ ind 1))))
      ((has-binding (curr-actual actual-params) new-state) ; variable as param input
       (addbinding-layers (curr-formal (formal-params closure) ind)
                          (lookup-layers (curr-actual actual-params) new-state)
                          (environment-func name closure return (next-params actual-params) old-state new-state (+ ind 1))))
      (else               (error "invalid input to the function parameters")))))

; updating the global variables from new state
(define update-global-vars
  (lambda (old-state new-state return old-base new-base)
    (cond
      ((not (null? (cdr old-state))) (cons (car old-state) (update-global-vars (cdr old-state) new-state return old-base new-base)))
      ((or (eq? return 'func-cont) (eq? return 'func-end)) (list (update-all-vars old-base new-base (lambda(v)v))))
      (else (list (update-helper old-base new-base (lambda(v)v)))))))
 
; helper method for update-global-vars; returns the updated base state
(define update-helper
  (lambda (old-base new-base return)
    (cond
      ((or (empty-state old-base) (empty-state new-base)) (return old-base))
      ((modify? old-base new-base return)
       (update-helper (next-state old-base) (next-state new-base)
                      (lambda (base) (return (updatebinding (get-var old-base) (lookup (get-var old-base) new-base) base)))))
      (else (update-helper (next-state old-base) (next-state new-base)
                           (lambda (base) (return (addbinding (get-var old-base) (get-val old-base) base))))))))
 
; helper method for checking if variable exists in both states & values not equal & modifiable following static scoping
(define modify?
  (lambda (old-base new-base indicator)
    (cond
      ((false? (var-exists? old-base new-base)) #f) ; variable does not already exist in both
      ((>= (+ 1 (length (get-vars old-base))) (length (get-vars new-base))) #t) ; static scoping
      ((eq? 'func-cont indicator) #t) ; exists & global vars need to be updated
      (else #f))))

(define update-all-vars
  (lambda (old-base new-base return)
    (cond
      ((empty-state old-base) (return old-base))
      ((modify? old-base new-base 'func-cont)
       (update-all-vars (next-state old-base) new-base
                      (lambda (base) (return (addbinding (get-var old-base) (lookup (get-var old-base) new-base) base)))))
      (else (update-all-vars (next-state old-base) new-base
                           (lambda (base) (return (addbinding (get-var old-base) (get-val old-base) base))))))))
 

; returns whether the variable given is already in scope for both old and new bases
(define var-exists?
  (lambda (old-base new-base)
    (if (and (find-var (get-var old-base) new-base) ; variable exists in both states & different values
                  (not (eq? (lookup (get-var old-base) new-base) (lookup (get-var old-base) old-base))))
        #t #f)))

; returns the base layer of the layers
(define base-layer
  (lambda (layers ind)
    (cond
      ((null? layers) layers)
      ((eq? ind '1) (curr-layer layers))
      (else (base-layer (cdr layers) (- ind 1))))))
; Helper functions for function functions
; For adding function definition
(define parse-func (lambda (stmt state next return continue break throw)
                     (parse-decl (func-closure stmt state) state next return continue break throw)))
; the closure of a function
(define func-closure (lambda (stmt state) (list 'var (func-name stmt) (list (func-params stmt) (func-body stmt)
                                                                            (static-scoping (func-name stmt) state)))))
; the function name
(define func-name (lambda (stmt) (cadr stmt)))
; the function body
(define func-body (lambda (stmt) (cadddr stmt)))
; formal parameter list for function
(define func-params (lambda (stmt) (caddr stmt)))

; returns true if input is number or a numerical expression else false
(define num? (lambda (expr) (if (or (number? expr) (and (pair? expr) (intexp? expr))) #t #f)))
; returns true if input is boolean or a boolean expression else false
(define bool? (lambda (expr) (if (or (boolean? expr) (and (pair? expr) (boolexp? expr))) #t #f)))
; returns the length of the given list of parameters
(define length (lambda (params) (if (null? params) 0 (+ 1 (length (cdr params))))))
; returns the specified function arguments (actual parameters)
(define arguments (lambda (funcall) (cddr funcall)))
; returns the function's body stored in its closure
(define closure-body (lambda (closure) (cons 'begin (cons 'func (cadr closure))))) ; prepare for parse-block
; returns the function's parameters stored in its closure
(define formal-params (lambda (closure) (car closure)))
; returns the first parameter
(define curr-actual (lambda (params) (car params)))
; returns the formal parameter at the given index
(define curr-formal (lambda (params ind) (if (eq? ind 0) (car params) (curr-formal (cdr params) (- ind 1)))))
; returns the list of next parameters
(define next-params (lambda (params) (cdr params)))
; returns whether statement is function call or not
(define funcall? (lambda (stmt) (if (and (list? stmt) (eq? (operator stmt) 'funcall)) #t #f)))


;============================================================================
; MAIN M-STATE FUNCTION
;============================================================================

; m-state changes the state based on certain statements
(define m-state
  (lambda (stmt state next return continue break throw)
    (cond
      ((not (list? stmt))               (next state))
      ((no-return? return stmt)         (next state))
      ((eq? (operator stmt) '=)         (parse-asgn (arg1 stmt) (arg2 stmt) state next return continue break throw))
      ((eq? (operator stmt) 'var)       (parse-decl stmt state next return continue break throw))
      ((eq? (operator stmt) 'while)     (parse-while stmt state next return (lambda (v) v) (lambda (v) v) throw))
      ((eq? (operator stmt) 'return)    (return (parse-return stmt state next return continue break throw)))
      ((eq? (operator stmt) 'if)        (parse-if stmt state next return continue break throw))
      ((eq? (operator stmt) 'begin)     (parse-block stmt state next return continue break throw)) ; For a block of code
      ((eq? (operator stmt) 'continue)  (continue (rmv-layer state)))
      ((eq? (operator stmt) 'break)     (break (rmv-layer state)))
      ((eq? (operator stmt) 'try)       (parse-try stmt state next return continue break throw))
      ((eq? (operator stmt) 'throw)     (throw (arg1 stmt) (rmv-layer state)))
      ((eq? (operator stmt) 'funcall)   (m-value stmt state next return continue break throw)) ; function call
      ((eq? (operator stmt) 'function)  (parse-func stmt state next return continue break throw)) ; declaring function definition
      ((not (roperand? stmt))           (m-state (loperand stmt) state next return continue break throw)) ; no right operand
      (else (m-state (loperand stmt) state
                     (lambda (v1) (m-state (roperand stmt) (next v1) (lambda (v2) v2) return continue break throw)) return continue break throw))))) ; Else, it's m-int or m-bool with args to update

; returns whether return function is set to no-return (for function calls without returning values)
(define no-return? (lambda (return stmt) (if (and (or (eq? return 'no-return) (eq? return 'func-cont)) (eq? (operator stmt) 'return)) #t #f)))

;============================================================================
; STATEMENT EVALUATION FUNCTIONS (i.e.parse-asgn, parse-decl, etc.)
;============================================================================

; parse-asgn takes in two statements and a state and adds the binding of the new value to the state 
(define parse-asgn 
  (lambda (var expr state next return continue break throw)
    (cond
      ((equal? (lookup-layers var state) 'not-initialized) (error "Variable not declared yet")) ; var not declared yet
      ((funcall? var)                      (error "attempted to assign value to a function call"))
      ((funcall? expr)                     (next (updatebinding-layers var (m-value expr state next return continue break throw) state)))
      ((number? expr)                      (next (updatebinding-layers var expr state))) ; assigning int
      ((boolean? expr)                     (next (updatebinding-layers var (bool-cvt expr) state))) ; assigning bool
      ((not (list? expr))                  (next (updatebinding-layers var (lookup-layers expr state) state))) ; variable
      ((bool-check (operator expr))        (next (updatebinding-layers var (make-boolean expr state) (update-state expr state (lambda(v)v) continue break throw))))                                             
      ((int-check (operator expr))         (next (updatebinding-layers var (m-int expr state (lambda(v)v) (lambda(v)v) continue break throw)
                                                                       (update-state expr state (lambda(v)v) continue break throw))))                                              
      ((eq? '= (operator expr))            (next (updatebinding-layers var ; handle assignment within assignment
                                                                       (lookup-layers (arg1 expr)
                                                                                      (parse-asgn (arg1 expr) (arg2 expr) state
                                                                                                   (lambda(v)v) (lambda(v)v) continue break throw))
                                                                       (parse-asgn (arg1 expr) (arg2 expr) state (lambda(v)v) (lambda(v)v) continue break throw))))
      (else                                (next (updatebinding-layers var expr state))))))

; parse-decl takes a declaration statement and adds it to the state with atom 'novalue to show it hasnt been given a value
(define parse-decl
  (lambda (stmt state next return continue break throw)
    (cond
      ((funcall? (arg1 stmt))              (error "attempted to assign value to a function call"))
      ((not (exp-arg stmt))                (next (addbinding-layers (arg1 stmt) 'novalue state))) ; no assignment
      ((funcall? (arg2 stmt))              (parse-asgn (arg1 stmt) (m-value (arg2 stmt) state next return continue break throw)
                                                       (addbinding-layers (arg1 stmt) 'novalue state) next
                                                       return continue break throw))
      (else                                (parse-asgn (arg1 stmt) (arg2 stmt) (addbinding-layers (arg1 stmt) 'novalue state)
                                                       next return continue break throw)))))


; parse-if evaluates if statements
(define parse-if
  (lambda (stmt state next return continue break throw)
    (cond
      ((m-bool (cond-stmt stmt) state next return continue break throw) (true-condition stmt state next return continue break throw))
      ((equal? 'no-else (else-stmt stmt))  (no-else-stmt stmt state next return continue break throw)) ; no else
      ((nested-if (else-stmt stmt))        (nested-if-stmt stmt state next return continue break throw)) ; nested if statements
      (else                                (false-condition stmt state next return continue break throw)))))  ; no nested if statements

; parse-while evaluates while statements
(define parse-while
  (lambda (stmt state next return continue break throw)
    (cond
      ((m-bool (condition stmt) state next return continue break throw)
       (loop stmt state next return continue break throw)) ; condition is true; continue looping 
      (else                                (exit-loop stmt state next return continue break throw))))) ; condition is false; stop loop

; parse-return evaluates return statements
(define parse-return
  (lambda (stmt state next return continue break throw) 
    (cond
      ((num-operation? stmt state)                        (m-int (return-val stmt) state next return continue break throw)) ; num operation
      ((bool-operation? stmt state)                       (make-boolean (return-val stmt) state)) ; bool operation
      ((number? (return-val stmt))                        (return-val stmt))
      ((boolean? (return-val stmt))                       (bool-cvt (return-val stmt)))
      ((funcall? (return-val stmt))                       (m-value (return-val stmt) state next return continue break throw))
      (else                                               (lookup-layers (return-val stmt) state)))))

; parse-block evaluates code blocks
(define parse-block
  (lambda (stmt layers next return continue break throw) ; assume layers are inputted
    (cond
      ((empty-stmt stmt)                            (next (rmv-layer layers))) ; remove the current layer at the end of block
      ((fun-block stmt)                             (parse-block (next-fun-stmts stmt) layers next return continue break throw))
      ((eq? 'begin (curr-stmt stmt))                (parse-block (next-stmts stmt) (add-layer layers) next return continue break throw))
      ((eq? 'continue (keyword (curr-stmt stmt)))   (continue (rmv-layer layers)))
      ((eq? 'break (keyword (curr-stmt stmt)))      (break (rmv-layer layers)))
      (else                                         (next-block stmt layers next return continue break throw))))) ; parse next in the block

; returns true if the current block is a function block
(define fun-block
  (lambda (stmt)
    (cond
      ((null? stmt) #f)
      ((and (eq? 'begin (curr-stmt stmt)) (eq? 'func (cadr stmt))) #t)
      (else #f))))
; returns the next statement to process for function blocks
(define next-fun-stmts (lambda (stmt) (if (null? stmt) stmt (cddr stmt))))

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
  (lambda (boolexp state next return continue break throw)
    (cond
      ((number? boolexp)                 boolexp)
      ((symbol? boolexp)                 (return-boolean boolexp state))
      ((intexp? boolexp)                 (m-int boolexp state next return continue break throw)) ; handle intexps nested in boolean expressions
      ((has-assignment boolexp state)    (handle-boolean-side-effects boolexp state)) ; if an assignment is nested in a boolexp
      ((eq? (operator boolexp) 'funcall) (m-value boolexp state next return continue break throw))
      ((eq? (operator boolexp) '&&)      (and         (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '||)      (or          (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '!)       (not         (left-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '==)      (equal?      (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '!=)      (not (equal? (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw))))
      ((eq? (operator boolexp) '<)       (<           (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '>)       (>           (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '<=)      (<=          (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw)))
      ((eq? (operator boolexp) '>=)      (>=          (loperand-boolean boolexp state next return continue break throw)
                                                      (right-boolexp boolexp state next return continue break throw))))))

; m-int takes in an expression and a state and returns a numeric value
(define m-int 
  (lambda (intexp state next return continue break throw)
    (cond
      ((number? intexp)                  intexp)
      ((symbol? intexp)                  (return-integer intexp state)) ; lookup variable value
      ((has-assignment intexp state)     (handle-int-side-effects intexp state next return continue break throw)); FIX ; if an assignment is nested in an intexp
      ((and (eq? (operator intexp) '-) (unary? intexp)) (* '-1 (m-int (loperand intexp) (left-intexp intexp state) next return continue break throw)))
      ((eq? (operator intexp) 'funcall)  (m-value intexp state next return continue break throw))
      ((eq? (operator intexp) '+)        (+ (loperand-intexp intexp state next return continue break throw)
                                            (right-intexp intexp state next return continue break throw)))
      ((eq? (operator intexp) '-)        (- (loperand-intexp intexp state next return continue break throw)
                                            (right-intexp intexp state next return continue break throw)))
      ((eq? (operator intexp) '*)        (* (loperand-intexp intexp state next return continue break throw)
                                            (right-intexp intexp state next return continue break throw)))
      ((eq? (operator intexp) '/)        (quotient (loperand-intexp intexp state next return continue break throw)
                                                   (right-intexp intexp state next return continue break throw)))
      ((eq? (operator intexp) '%)        (remainder (loperand-intexp intexp state next return continue break throw)
                                                    (right-intexp intexp state next return continue break throw))))))
 
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

; add a binding if it does not already exist; update if bining already exists
(define addbinding-layers
  (lambda (var val layers)
    (cond
      ((empty-layers layers) layers) ; state is empty; return original state
      ((false? (has-binding var layers)) ; var not in the layers yet; add to the top layer
       (cons (addbinding var val (curr-layer layers)) (rmv-layer layers)))
      (else (updatebinding-layers var val layers))))) ; update existing binding if exists

; remove a binding from the state while accounting for multiple layers
(define removebinding-layers
  (lambda (var layers)
    (cond
      ((empty-layers layers) layers) ; state is empty; return original state
      ((false? (find-var var (curr-layer layers))) (cons (curr-layer layers) (removebinding-layers var (next-layers layers))))
      (else (cons (removebinding var (curr-layer layers)) (rmv-layer layers))))))

; update a binding while accounting for layers; update in the layer the variable exists in
; will not update the layers (state) if var does not exists already
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
    (bool-cvt (m-bool expr state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)))))

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
    (cond
      ((and (eq? 'funcall (operator (curr-stmt stmt))) (eq? return 'func-cont))
       (m-state (curr-stmt stmt) layers
                (lambda (state) (end-of-funcall (next-stmts stmt) layers state next 'func-cont continue break throw))
                return continue break throw))
      ((eq? 'funcall (operator (curr-stmt stmt)))
       (m-state (curr-stmt stmt) layers
                (lambda (state) (end-of-funcall (next-stmts stmt) layers state next 'no-return continue break throw))
                'no-return continue break throw)) ; return functions different; 'func-cont if next is also function call
                                                  ; in the next continuation function, update state to have updated global variables                       
      (else
       (m-state (curr-stmt stmt) layers
                (lambda (state) (parse-block (next-stmts stmt) state next return continue break throw))
                return continue break throw)))))

; adjusts the next function dependong on whether the next function is a function call or not
(define end-of-funcall
  (lambda (stmt old-state new-state next return continue break throw)
    (cond
      ((eq? 'funcall (operator (curr-stmt stmt))) ; next is also a func call after a func call; set return to 'func-cont
       (parse-block stmt (update old-state new-state) next 'func-cont continue break throw)) ; has to include the function's definition, etc.
      (else (parse-block stmt (update old-state new-state) next (lambda(v)v) continue break throw))))) ; resetting return function for when it's not func call anymore after

; return the updated function; updates all global variables
; old-state is the overall block's function, and new-state contains the updated global variables
(define update
  (lambda (old-state new-state)
    (update-global-vars old-state new-state 'func-end (base-layer old-state (length old-state)) (base-layer new-state (length new-state)))))


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
       (define new-throw (lambda (e st) (parse-block (catch-block stmt) (catch-state stmt e st continue break throw)
                                                     new-next return continue break throw)))
       (m-state try-block state new-next return continue break new-throw))) ; beginning of try/catch/finally

; helper that adds binding for the specified error name
(define catch-state
  (lambda (stmt val state continue break throw)
    (cond
      ((null? (catch-block stmt)) state) ; there's no catch block; no change
      (else (parse-asgn (catch-var stmt) val (addbinding-layers (catch-var stmt) 'novalue (add-layer state)) (lambda(v)v) (lambda(v)v) continue break throw)))))

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
  (lambda (boolexp state next return continue break throw)
    (m-bool (loperand boolexp) state next return continue break throw)))

; returns a continuation to evaluate the right part of the boolean expression
(define right-boolexp
  (lambda (boolexp state next return continue break throw)
    (cond
      ((funcall? (roperand boolexp)) (m-value (roperand boolexp) state next return continue break throw))
      ((funcall? (loperand boolexp)) (m-bool (roperand boolexp) state next return continue break throw))
      (else                          (m-bool (roperand boolexp) (m-state (loperand boolexp) state (lambda (v) v) (lambda (v) v) continue break throw)
                                             next return continue break throw)))))

; returns a continuation to evaluate the left part of a boolean expression
(define left-boolexp
  (lambda (boolexp state next return continue break throw)
    (m-bool (loperand boolexp) (m-state (loperand boolexp) state (lambda (v) v) (lambda (v) v) continue break throw)
             next return continue break throw)))

; returns whether theres an assignment statement in either the left/right operand of the boolean expression
(define has-assignment
  (lambda (boolexp state)
    (or (is-asgn (loperand boolexp)) ; handle assignment statements within booleans
           (and (roperand? boolexp) (is-asgn (roperand boolexp))))))

; ******************** M-INT HELPERS *************************

; helper that handles assignment statements within integers
(define handle-int-side-effects
  (lambda (intexp state next return continue break throw)
    (m-int (new-stmt (operator intexp) (value-get (asgn-var (loperand intexp)) (m-state (loperand intexp) state (lambda (v) v) (lambda (v) v) continue break throw))
                        (value-get (asgn-var (roperand intexp)) (m-state (roperand intexp) (m-state (loperand intexp) state (lambda (v) v) (lambda (v) v) continue break throw)
                                                                         (lambda (v) v) (lambda (v) v) continue break throw))) state)))

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
  (lambda (intexp state next return continue break throw)
    (m-int (loperand intexp) state next return continue break throw)))

; returns a continuation to evaluate the right integer operand
(define right-intexp
  (lambda (intexp state next return continue break throw)
    (cond
      ((funcall? (roperand intexp)) (m-value (roperand intexp) state next return continue break throw))
      ((funcall? (loperand intexp)) (m-int (roperand intexp) state next return continue break throw))
      (else                         (m-int (roperand intexp) (update-state (loperand intexp) state (lambda(v)v) continue break throw); FIX?
                                           next return continue break throw)))))

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

; returns updated states without function calls; for m-int and m-bool
(define update-state
  (lambda (stmt state return continue break throw)
    (cond
      ((or (not (list? stmt)) (funcall? stmt)) (return state))                ; number, symbol, function calls => no update
      ((or (int-check (operator stmt)) (bool-check (operator stmt)))          ; int or bool operations
       (update-state (loperand stmt) state
                     (lambda (new-state)
                       (return (update-state (roperand stmt) new-state (lambda (new-state2) new-state2)
                                             continue break throw))) continue break throw))
      (else (return (m-state stmt state (lambda (v) v) continue break throw)))))) ; update state

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
    (cond
      ((empty-state state) state)
      ((eq? var (get-var state)) (addbinding var val (removebinding var state))) ;remove existing bindings first & add new binding
      (else (addbinding (get-var state) (get-val state) (updatebinding var val (next-state state)))))))

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
; TEST CASES
;============================================================================

; test cases for part 1

#|
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
|#


(check-expect (interpret "MakeTestsPart3/test1c.txt")   10)
(check-expect (interpret "MakeTestsPart3/test2c.txt")   14)
(check-expect (interpret "MakeTestsPart3/test3c.txt")   45)
(check-expect (interpret "MakeTestsPart3/test4c.txt")   55)
(check-expect (interpret "MakeTestsPart3/test5c.txt")   1)
(check-expect (interpret "MakeTestsPart3/test6c.txt")   115)
(check-expect (interpret "MakeTestsPart3/test7c.txt")   'true)
(check-expect (interpret "MakeTestsPart3/test8c.txt")   20)
(check-expect (interpret "MakeTestsPart3/test9c.txt")   24)
(check-expect (interpret "MakeTestsPart3/test10c.txt")  2)
(check-expect (interpret "MakeTestsPart3/test11c.txt")  35)
(check-error  (interpret "MakeTestsPart3/test12c.txt")  "mismatched parameters and arguments")
(check-expect (interpret "MakeTestsPart3/test13c.txt")  90)
(check-expect (interpret "MakeTestsPart3/test14c.txt")  69)
(check-expect (interpret "MakeTestsPart3/test15c.txt")  87)
(check-expect (interpret "MakeTestsPart3/test16c.txt")  64)
(check-error  (interpret "MakeTestsPart3/test17c.txt")  "using before declaring")
(check-expect (interpret "MakeTestsPart3/test18c.txt")  125)
(check-expect (interpret "MakeTestsPart3/test19c.txt")  100)
(check-expect (interpret "MakeTestsPart3/test20c.txt")  2000400)
; ADDITIONAL CHALLENGES:
;(check-expect (interpret "MakeTestsPart3/test21c.txt")  3421)
;(check-expect (interpret "MakeTestsPart3/test22c.txt")  20332)
;(check-expect (interpret "MakeTestsPart3/test23c.txt")  21)

