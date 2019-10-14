
;; Parsed expression datatypes

(define-datatype expression expression?
    [var-exp 
        (var symbol?)
    ]
    [lit-exp
        (val datum?)
    ]
    [lambda-body-not-list-exp
        (args (list-of symbol?))
        (body (list-of expression?))
    ]
    [lambda-body-is-list-exp
        (args (list-of symbol?))
        (body expression?)
    ]
    [lambda-no-args-exp
        (body (list-of expression?))
    ]
    [app-exp
        (rator expression?)
        (rands (list-of expression?))
    ]
    [if-exp-no-just
        (pred expression?)
        (then_case expression?)
    ]
    [if-exp
        (pred expression?)
        (then_case expression?)
        (just_in_case expression?)
    ]
    [let-exp
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [let*-body-not-list-exp
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [let*-body-is-list-exp
        (vars (list-of pair?))
        (body expression?)
    ]
    [named-let-exp
        (name symbol?)
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [letrec-exp
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [set!-exp
        (var symbol?)
        (body expression?)
    ]
)

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
    [prim-proc
        (name symbol?)]
     [lambda-body-not-list-proc
        (args (list-of symbol?))
        (body (list-of expression?))
        (local-env environment?)
    ]
    [lambda-body-is-list-proc
        (args (list-of symbol?))
        (body expression?)
        (local-env environment?)
    ]
    [lambda-no-args-proc
        (body (list-of expression?))
        (local-env environment?)

    ]
)
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))