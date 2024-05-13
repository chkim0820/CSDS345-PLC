#lang racket
(require "classParser.rkt")
(require test-engine/racket-tests)

; Authors: Chae Kim, Amir Aliyu
; Date: April 4, 2024
; Description: Flow Control Interpreter

;============================================================================
; INTERPRET (MAIN) FUNCTION
;============================================================================

(define interpret
  (lambda (filename classname)
    (parse-prog (parser filename) (string->symbol classname) '((()()))))) ; the layer contains the initial state (expressed with [])

;============================================================================
; PROGRAM PARSER 
;============================================================================
; parse-program goes through the entire program and creates a closure for every class, returns all the closures
; parse-prog takes a list representing a syntax tree and returns the proper value (or error).
(define parse-prog
  (lambda (prog classname state)
    (cond
      ((null? prog) (m-class classname state ; run the main function in the specified class
                             (lambda (v) v) ; initializing next
                             (lambda (v) v)
                             (lambda (v) (error "invalid continue"))
                             (lambda (v) (error "invalid break"))
                             (lambda (v1 v2) (error "error thrown"))))
      (else (outer-classes (curr-line prog) state
                           (lambda (new-state) (parse-prog (next-lines prog) classname new-state))
                           (lambda (v) v)
                           (lambda (v) (error "invalid continue"))
                           (lambda (v) (error "invalid break"))
                           (lambda (v1 v2) (error "error thrown")))))))

(define m-class
  (lambda (classname state next return continue break throw)
    (m-value '(funcall main) (new-state (caddr (lookup-layers classname state)) state) next return continue break throw)))

(define new-state
  (lambda (state classes)
    (cond
      ((null? state) classes)
      (else (cons (curr-layer state) (addbinding-layers 'this state (new-state (next-layers state) classes)))))))

(define outer-classes
  (lambda (stmt state next return continue break throw)
    (cond
      ((eq? (operator stmt) 'class) (parse-class stmt state next return continue break throw))
      (else (error "Only classes allowed on the base layer")))))

(define parse-class
  (lambda (stmt state next return continue break throw)
    (parse-decl (class-closure stmt '[((true false) (#t #f))]) state next return continue break throw)))

(define class-closure
  (lambda (stmt state)
    (list 'var (class-name stmt) (list (class-superclass stmt) ; superclass, variables, and function closure
                                       (class-body stmt)
                                       (run-outer-layer (class-body stmt) state)))))

(define run-outer-layer
  (lambda (stmt state)
    (cond
      ((null? stmt) state)
      (else (outer-layer (curr-line stmt) state
                         (lambda (new-state) (run-outer-layer (next-lines stmt) new-state))
                         (lambda (v) v)
                         (lambda (v) (error "invalid continue"))
                         (lambda (v) (error "invalid break"))
                         (lambda (v1 v2) (error "error thrown")))))))

; outer-layer does m-state functions for variable declarations and function definitions
(define outer-layer
  (lambda (stmt state next return continue break throw)
    (cond
      ((not (list? stmt))                     (next state)) ; at the end; will call main-method
      ((eq? (operator stmt) 'var)             (parse-decl stmt state next return continue break throw))
      ((eq? (operator stmt) 'function) (parse-func stmt state next return continue break throw))
      ((eq? (operator stmt) 'static-function) (parse-func stmt state next return continue break throw))
      (else                                   (error "Only function calls and assignments allowed on the base layer"))))) ; Else, it's m-int or m-bool with args to update

; helper function for next program
(define curr-line (lambda (prog) (if (null? prog) prog (car prog))))
(define next-lines (lambda (prog) (if (null? prog) prog (cdr prog))))

;============================================================================
; CLASS PROCESSING
;============================================================================
; instance closure - return class and variables of the class

(define instance-closure
  (lambda (stmt)
    (list (class-name stmt) (class-vars (class-body stmt)))))

(define dot-expression-instance
  (lambda (dotexp state)
    (instance-closure state)))

; the class name
(define class-name (lambda (stmt) (cadr stmt)))

; the class body (includes instance fields, functions)
(define class-body (lambda (stmt) (cadddr stmt)))

; the class superclass
(define class-superclass
  (lambda (stmt)
    (cond
     ((has-superclass stmt)  (car (cdr (car (cdr (cdr stmt))))))
     (else                    '()))))

(define has-superclass
  (lambda (stmt)
    (cond
      ((null? (car (cdr (cdr stmt)))) #f)
      (else                           #t))))

; add function names and closures for each class function
(define class-function-closures
  (lambda (functions state)
    (cond
      ((null? functions)              functions)
      (else                           (cons (func-closure (operator functions) state) (class-function-closures (cdr functions) state))))))

; find class functions from within a class-body
(define class-functions
  (lambda (body)
    (cond
      ((null? body)              body)
      ((eq? (car (operator body)) 'function)         (cons (operator body) (class-functions (cdr body))))
      ((eq? (car (operator body)) 'static-function)  (cons (operator body) (class-functions (cdr body))))
      (else                                          (class-functions (cdr body))))))

; find instance variables from within a class-body
(define class-vars
  (lambda (body)
    (cond
      ((null? body)              body)
      ((eq? (car (operator body)) 'var) (cons (operator body) (class-vars (cdr body))))
      (else                                          (class-vars (cdr body))))))    

; helper function to check if there are variables declared within the class
(define has-instance-vars
  (lambda (stmt)
    (cond
      ((eq? (operator (car (class-body stmt))) 'var)     #t) 
      (else                                              #f))))

;============================================================================
; FUNCTION PROCESSING; added for project 3!!!
;============================================================================
; Function call syntax: '(function name formal-params body)

; outer-layer does m-state functions for variable declarations and function definitions

; m-value for function calls (funcalls)
; change this to handle dot operator - change func-name to return the actual func-name
(define m-value
  (lambda (funcall state next return continue break throw)
    (define closure (determine-closure (func-name funcall) state)) ; defining the function's closure (param body func)
    (m-state (closure-body closure) (environment closure (arguments funcall) state return) ; call the function in m-state
             (lambda (st) (next (adjust-states st state (- (depth state) (depth st))))) return continue break throw)))

(define determine-closure
  (lambda (func-name state)
    (cond
      ((not (list? func-name)) (lookup-layers func-name state))
      ((eq? (operator func-name) 'dot) (lookup-layers (arg2 func-name) (lookup-layers (arg1 func-name) state)))
      (else (error "debugging")))))

; adjusting scope after function call
(define adjust-states
  (lambda (funcall-st overall-st counter)
    (cond
      ((empty-layers funcall-st) funcall-st)
      ((< counter 0) funcall-st)
      ((eq? counter 0) (cons (add-after-funcall (curr-layer overall-st) (curr-layer funcall-st))
                             (adjust-states (next-layers funcall-st) (next-layers overall-st) 0)))
      ((> counter 0) (cons (car overall-st) (adjust-states funcall-st (next-layers overall-st) (- counter 1)))))))

; returns the depth of the layers
(define depth
  (lambda (layers)
    (cond
      ((empty-layers layers) 0)
      (else (+ 1 (depth (next-layers layers)))))))

; add nonexisting variables
(define add-after-funcall
  (lambda (add-from-layer add-to-layer)
    (add-after-helper add-from-layer add-to-layer (lambda (v) v))))

(define add-after-helper
  (lambda (add-from add-to return)
    (cond
      ((or (empty-state add-from) (empty-state add-to)) (return add-to))
      ((false? (find-var (get-var add-from) add-to))
       (add-after-helper (next-state add-from) add-to
                      (lambda (base) (return (addbinding (get-var add-from) (get-val add-from) base)))))
      (else (add-after-helper (next-state add-from) (next-state add-to)
                           (lambda (base) (return (addbinding (get-var add-to) (get-val add-to) base))))))))

; helper to check if the funcall is a dot operator:
(define has-dot
  (lambda (funcall)
    (cond
      ((eq? (list? (car (cdr funcall))) #f) #f)
      ((eq? (car (car (cdr funcall))) 'dot) #t)
      (else #f))))

; helper to check if it's this.
(define is-this-dot
  (lambda (dotexp)
    (cond
      ((eq? 'this (car (cdr dotexp))) #t)
      (else                              #f))))
 

; if the function call is a dot operator, use myfunc-name funcall, else, use m-value 
(define m-value-dot
  (lambda (funcall state next return continue break throw)
    (define closure (lookup-layers (myfunc-name funcall) state)) ; defining the function's closure (param body func)
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
      ;((and (eq? ind 0) (not (eq? (length (formal-params closure)) (length actual-params))))
      ; (error "mismatched parameters and arguments"))
      ;; For initializing new state for running a function with static scoping, add the current layer of the outer function.
      ((null? actual-params) (add-layer (update-global-vars old-state new-state name closure (- (- (depth new-state) 1) (depth old-state)) return)))
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
; old state is the definition in the closure
; new state is the state up to the point up to the funcall
(define update-global-vars
  (lambda (old-state new-state func-name closure counter return)
    (cond
      ((empty-layers old-state) old-state)
      ;((< counter 0) (cons (add-after-funcall (curr-layer old-state) (curr-layer new-state)) (update-global-vars (next-layers old-state) (next-layers new-state) counter return)))
      ((and (eq? counter 0) (or (eq? return 'func-cont) (eq? return 'func-end))) (cons (add-after-funcall (curr-layer old-state) (curr-layer new-state)) (update-global-vars (next-layers old-state) (next-layers new-state) func-name closure (- counter 1) return)))
      ((eq? counter 0) (cons (update-helper (add-curr-func-def func-name closure (curr-layer old-state)) (curr-layer new-state) (lambda (v) v)) (next-layers new-state)));(update-global-vars (next-layers old-state) (next-layers new-state) (- counter 1) return)))
      ((> counter 0) (update-global-vars old-state (next-layers new-state) func-name closure (- counter 1) return)))))

; add the function definition to the state
(define add-curr-func-def
  (lambda (func-name closure state)
    (addbinding func-name closure state)))

; helper method for update-global-vars; returns the updated base state
(define update-helper
  (lambda (old-base new-base return)
    (cond
      ((or (empty-state old-base) (empty-state new-base)) (return old-base))
      ((find-var (get-var old-base) new-base)
       (update-helper (next-state old-base) new-base
                      (lambda (base) (return (addbinding (get-var old-base) (lookup (get-var old-base) new-base) base)))))
      (else (update-helper (next-state old-base) new-base return)))))
 
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

; Add "this" as an additional parameter to the formal parameter list in each (non-static) function's closure.
; the closure of a function
; add the class that this function belongs to to the function closure
; func-closure with this (made it its own version bc it was messing w parameters, can add it in later if necessary)
(define func-closure-this (lambda (stmt state) (list 'var (func-name stmt) (list (cons 'this (func-params stmt)) (func-body stmt)
                                                                            (static-scoping (func-name stmt) state)))))
; func-closure no this:
(define func-closure (lambda (stmt state) (list 'var (func-name stmt) (list (func-params stmt) (func-body stmt)
                                                                            (static-scoping (func-name stmt) state)))))

; the function name but accommodating dots
(define myfunc-name (lambda (stmt) (car (cdr (cdr (car (cdr stmt)))))))

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
      ((not (list? stmt))                   (next state))
      ((no-return? return stmt)             (next state))
      ((eq? (operator stmt) '=)             (parse-asgn (arg1 stmt) (arg2 stmt) state next return continue break throw))
      ((eq? (operator stmt) 'var)           (parse-decl stmt state next return continue break throw))
      ((eq? (operator stmt) 'while)         (parse-while stmt state next return (lambda (v) v) (lambda (v) v) throw))
      ((eq? (operator stmt) 'return)        (return (parse-return stmt state next return continue break throw)))
      ((eq? (operator stmt) 'if)            (parse-if stmt state next return continue break throw))
      ((eq? (operator stmt) 'begin)         (parse-block stmt state next return continue break throw)) ; For a block of code
      ((eq? (operator stmt) 'continue)      (continue (rmv-layer state)))
      ((eq? (operator stmt) 'break)         (break (rmv-layer state)))
      ((eq? (operator stmt) 'try)           (parse-try stmt state next return continue break throw))
      ((eq? (operator stmt) 'throw)         (throw (arg1 stmt) (rmv-layer state)))
      ((eq? (operator stmt) 'funcall)       (m-value stmt state next return continue break throw)) ; function call
      ((eq? (operator stmt) 'function)      (parse-func stmt state next return continue break throw)) ; declaring function definition
      ((eq? (operator stmt) 'static-function)  (parse-func stmt state next return continue break throw)) ; declaring function definition
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
      ((and (list? var) (eq? (operator var) 'dot)) (next (updatebinding-layers (arg1 var) (updatebinding-layers var (lookup-layers (arg2 var) (lookup-layers (arg1 var) state))) state)))
      ((equal? (lookup-layers var state) 'not-initialized) (error "Variable not declared yet")) ; var not declared yet
      ((funcall? var)                      (error "attempted to assign value to a function call"))
      ((funcall? expr)                     (m-value expr state
                                                    (lambda (st) (next (addbinding-layers var (m-value expr state 'debug return continue break throw) st)))
                                                    'no-return continue break throw))
      ((init-class? expr)                  (next (updatebinding-layers var (caddr (lookup-layers (which-class expr) state)) state)))
      ((number? expr)                      (next (updatebinding-layers var expr state))) ; assigning int
      ((boolean? expr)                     (next (updatebinding-layers var (bool-cvt expr) state))) ; assigning bool
      ((not (list? expr))                  (next (updatebinding-layers var (lookup-layers expr state) state))) ; variable
      ((bool-check (operator expr))        (next (updatebinding-layers var (make-boolean expr state) (update-state expr state (lambda(v)v) continue break throw))))                                             
      ((int-check (operator expr))         (next (updatebinding-layers var (m-int expr state (lambda(v)v) (lambda(v)v) continue break throw)
                                                                       (update-state expr state (lambda(v)v) continue break throw))))                                              
      ((eq? '= (operator expr))            (next (updatebinding-layers var (lookup-layers (arg1 expr) (parse-asgn (arg1 expr) (arg2 expr) state
                                                                                                                  (lambda(v)v) (lambda(v)v) continue break throw))
                                                                       (parse-asgn (arg1 expr) (arg2 expr) state (lambda(v)v) (lambda(v)v) continue break throw))))
      ; check for new operator
      ((eq? #t (is-new expr))              (next (updatebinding-layers var (cdr state) state)))
      (else                                (next (updatebinding-layers var expr state))))))

(define init-class?
  (lambda (expr)
    (cond
      ((not (list? expr)) #f)
      ((eq? (operator expr) 'new) #t)
      (else #f))))

(define which-class (lambda (expr) (cadr expr)))

; helper to check if expr is new
(define is-new
  (lambda (expr)
    (cond
      ((and (eq? (list? expr) #t) (eq? 'new (car expr))) #t)
      (else                   #f))))
  

; parse-decl takes a declaration statement and adds it to the state with atom 'novalue to show it hasnt been given a value
(define parse-decl
  (lambda (stmt state next return continue break throw)
    (cond
      ((funcall? (arg1 stmt))              (error "attempted to assign value to a function call"))
      ((not (exp-arg stmt))                (next (addbinding-layers (arg1 stmt) 'novalue state))) ; no assignment
      ((funcall? (arg2 stmt))              (parse-asgn (arg1 stmt) (m-value (arg2 stmt) state next return continue break throw)
                                                       (addbinding-layers (arg1 stmt) 'novalue state) next
                                                       return continue break throw))
      ; say if (new) is on the left side, add the instance closure of the class to the state ? 
      (else                                (parse-asgn (arg1 stmt) (arg2 stmt) (addbinding-layers (arg1 stmt) (instance-closure (car (parser "testprogram.txt"))) state)
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
      ((empty-stmt stmt)                            (next layers)) ; remove the current layer at the end of block
      ((fun-block stmt)                             (parse-block (next-fun-stmts stmt) layers (lambda (new-st) (next (pushdown-layer stmt new-st))) return continue break throw))
      ((eq? 'begin (curr-stmt stmt))                (parse-block (next-stmts stmt) (add-layer layers) (lambda (new-st) (next (pushdown-layer stmt new-st))) return continue break throw))
      ((eq? 'continue (keyword (curr-stmt stmt)))   (continue (pushdown-layer layers)))
      ((eq? 'break (keyword (curr-stmt stmt)))      (break (pushdown-layer layers)))
      (else                                         (next-block stmt layers next return continue break throw))))) ; parse next in the block
; for names not passing down, make next continuation where those names are taken out (var and formal params)
(define takeout-vars
  (lambda (list state)
    (cond
      ((or (null? list) (empty-state state)) state)
      ((find-var (car list) state) (takeout-vars (cdr list) (removebinding (car list) state)))
      (else (addbinding (get-var state) (get-val state) (takeout-vars (cdr list) (next-state state)))))))

(define list-to-eliminate
  (lambda (stmt)
    (cond
      ((null? stmt) '())
      ((fun-block stmt) (list-to-eliminate (next-fun-stmts stmt)))
      ((eq? 'begin (curr-stmt stmt)) (list-to-eliminate (next-stmts stmt)))
      ((eq? 'var (keyword (curr-stmt stmt))) (cons (arg1 (curr-stmt stmt)) (list-to-eliminate (next-stmts stmt))))
      (else (list-to-eliminate (next-stmts stmt))))))

; push down the current layer's values to the lower layers
(define pushdown-layer
  (lambda (stmt layers)
    (pushdown-helper (takeout-vars (list-to-eliminate stmt) (curr-layer layers)) (next-layers layers))))

(define pushdown-helper
  (lambda (top-layer bottom-layers)
    (cond
      ((empty-state top-layer) bottom-layers)
      ((has-binding (get-var top-layer) bottom-layers)
       (pushdown-helper (next-state top-layer) (updatebinding-all-layers (get-var top-layer) (get-val top-layer) bottom-layers (lambda(v)v)))) ; change to updatebinding-layers?
      (else (pushdown-helper (next-state top-layer) bottom-layers)))))

(define updatebinding-all-layers
  (lambda (var val layers return)
    (cond
      ((empty-layers layers) (return layers))
      ((find-var var (curr-layer layers)) (updatebinding-all-layers var val (next-layers layers)
                                                                    (lambda (v) (return (cons (updatebinding var val (curr-layer layers)) v)))))
      (else (updatebinding-all-layers var val (next-layers layers) (lambda (v) (return (cons (curr-layer layers) v))))))))

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
      ((eq? (operator boolexp) 'dot)     (lookup-layers (arg2 boolexp) (lookup-layers (arg1 boolexp) state)))
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
      ;((dotexp? intexp)                    (return-integer (var-dot intexp) state))
      ((symbol? intexp)                  (return-integer intexp state)) ; lookup variable value
      ((has-assignment intexp state)     (handle-int-side-effects intexp state next return continue break throw)); FIX ; if an assignment is nested in an intexp
      ((and (eq? (operator intexp) '-) (unary? intexp)) (* '-1 (m-int (loperand intexp) (left-intexp intexp state) next return continue break throw)))
      ((eq? (operator intexp) 'funcall)  (m-value intexp state next return continue break throw))
      ((eq? (operator intexp) 'dot)      (lookup-layers (arg2 intexp) (lookup-layers (arg1 intexp) state)))
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

; var-dot takes in an dotexp and returns the variable within the dot
(define var-dot (lambda (dotexp) (car (cdr (cdr dotexp)))))
 
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
      ((find-var var (curr-layer layers)) (updatebinding-layers var val layers))
      (else (cons (addbinding var val (curr-layer layers)) (rmv-layer layers)))))) ; add to the top layer

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
      ((eq? 'funcall (operator (curr-stmt stmt)))
      (m-state (curr-stmt stmt) layers
                (lambda (state) (parse-block (next-stmts stmt) state next return continue break throw))
                'no-return continue break throw))
      (else (m-state (curr-stmt stmt) layers
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
; modify to allow dots

;       ;((list? intexp)                    (return-integer (var-dot intexp) state))

(define return-integer
  (lambda (intexp state)
    (cond
      ((eq? (dotexp? intexp) #t) (return-integer (var-dot intexp) state))
      ((equal? (lookup-layers intexp state) 'novalue) (error "using before assigning"))
      (else (lookup-layers intexp state))))) ; lookup variable value

; return whether or not this is a dotexp
(define dotexp?
  (lambda (intexp)
    (cond
      ((and (eq? (list? intexp) #t) (eq? (car intexp) 'dot)) #t)
      (else #f))))

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
      ; this had one less necessary parameter - next? 
      (else (return (m-state stmt state (lambda (v) v) return continue break throw)))))) ; update state

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

#|
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
(check-expect (interpret "MakeTestsPart3/test20c.txt")  2000400) |#
; ADDITIONAL CHALLENGES:
;(check-expect (interpret "MakeTestsPart3/test21c.txt")  3421)
;(check-expect (interpret "MakeTestsPart3/test22c.txt")  20332)
;(check-expect (interpret "MakeTestsPart3/test23c.txt")  21)

; PART 4 TEST CASES
(check-expect (interpret "MakeTestsPart4/test1d.txt")   15) ; works
(check-expect (interpret "MakeTestsPart4/test2d.txt")   12) ; works
(check-expect (interpret "MakeTestsPart4/test3d.txt")   125)
(check-expect (interpret "MakeTestsPart4/test4d.txt")   36)
(check-expect (interpret "MakeTestsPart4/test5d.txt")   54)
(check-expect (interpret "MakeTestsPart4/test6d.txt")   110)
#|(check-expect (interpret "MakeTestsPart4/test7d.txt")   26)
(check-expect (interpret "MakeTestsPart4/test8d.txt")   117)
(check-expect (interpret "MakeTestsPart4/test9d.txt")   32)
(check-expect (interpret "MakeTestsPart4/test10d.txt")   15)
(check-expect (interpret "MakeTestsPart4/test11d.txt")   123456)
(check-expect (interpret "MakeTestsPart4/test12d.txt")   5285)
(check-expect (interpret "MakeTestsPart4/test13d.txt")   -716)|#
; THE FOLLOWING ARE OPTIONAL:
#|
; (overloaded functions, call-by-reference, expressions with side-effects)
(check-expect (interpret "MakeTestsPart4/test21d.txt")   530)
(check-expect (interpret "MakeTestsPart4/test22d.txt")   66)
(check-expect (interpret "MakeTestsPart4/test23d.txt")   1026)
(check-expect (interpret "MakeTestsPart4/test24d.txt")   2045)
; tests for implementing static functions & variables
(check-expect (interpret "MakeTestsPart4/test31d.txt")   20)
(check-expect (interpret "MakeTestsPart4/test32d.txt")   350)
(check-expect (interpret "MakeTestsPart4/test33d.txt")   615)
(check-expect (interpret "MakeTestsPart4/test34d.txt")   16)
(check-expect (interpret "MakeTestsPart4/test35d.txt")   100)
(check-expect (interpret "MakeTestsPart4/test36d.txt")   420)
(check-error  (interpret "MakeTestsPart4/test37d.txt")   "no this")
; tests for abstract methods
(check-expect (interpret "MakeTestsPart4/test41d.txt")   300)
(check-error  (interpret "MakeTestsPart4/test42d.txt")   "non-overriden abstract method")
; tests for non-default constructors
(check-expect (interpret "MakeTestsPart4/test51d.txt")   417)
(check-expect (interpret "MakeTestsPart4/test52d.txt")   10)
(check-expect (interpret "MakeTestsPart4/test53d.txt")   48)
(check-expect (interpret "MakeTestsPart4/test54d.txt")   1659) |#
