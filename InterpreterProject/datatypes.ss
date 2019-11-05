
;; Parsed expression datatypes

(define-datatype expression expression?
    [var-exp 
        (var symbol?)
    ]
    [lit-exp
        (val scheme-value?)
    ]
    [lambda-exp
        (args lambda-value?)
        (body (list-of expression?))
    ]
    [lambda-variable-args-exp
        (args variable-args?)
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
    [named-let-exp
        (name symbol?)
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [letrec-exp
        (proc-names (list-of symbol?))
        (idss (list-of (list-of symbol?)))
        (bodiess (list-of (list-of expression?)))
        (letrec-bodies (list-of expression?))
        ; was:
        ; (vars (list-of pair?))
        ; (body (list-of expression?))
    ]
    [set!-exp
        (var symbol?)
        (body expression?)
    ]
    [cond-exp
        (bodies (list-of scheme-value?))
    ]
    [begin-exp
        (bodies (list-of scheme-value?))
    ]
    [while-exp
        (test-exp expression?)
        (bodies (list-of expression?))
    ]
    [case-exp
        (condition scheme-value?)
        (bodies (list-of scheme-value?))
    ]
    [or-exp 
        (bodies (list-of expression?))
    ]
    [define-exp 
        (name symbol?)
        (definition expression?)
    ]
)
	
(define-datatype environment environment?
    (empty-env-record)
    (extended-env-record
        (syms (list-of variable-args?))
        (vals (list-of scheme-value?))
        (env environment?))
    (recursively-extended-env-record
        (proc-names (list-of symbol?))
        (idss (list-of symbol?))
        (bodiess (list-of (list-of expression?)))
        (env environment?)
    )

   
)
   
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
    [prim-proc
        (name symbol?)]
    [closure
        (arg-names closure-args?)
        (bodies (list-of expression?))
        (local-env environment?)]
)
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define lambda-value?
    (lambda (x) (or ((list-of (list-of symbol?)) x) ((list-of symbol?) x)))
)

; Box ADT

(define cell 
    (lambda (v)
        (box v)
    )
)

(define cell?
    (lambda (x)
        (box? x)
    )
)

(define cell-ref unbox)

(define cell-set! set-box!)

; Reference ADT

(define deref cell-ref)

(define set-ref! cell-set!)