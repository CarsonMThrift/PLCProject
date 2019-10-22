; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
; (define 1st car)
; (define 2nd cadr)
; (define 3rd caddr)

; (define parse-exp         
;   (lambda (datum)
;     (cond
;      [(symbol? datum) (var-exp datum)]
;      [(number? datum) (lit-exp datum)]
;      [(pair? datum)
;       (cond
       
;        [else (app-exp (parse-exp (1st datum))
; 		      (map parse-exp (cdr datum)))])]
;      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define datum?
    (lambda (d)
        (or (null? d)
            (number? d) 
            ; (symbol? d) 
            (vector? d)
            (boolean? d)
            (char? d)
            (string? d)
            ; ((list-of datum?) d) ; Is this right for quoted lists? -> car = quote?
        )
    )
)
(define scheme-value?
    (lambda (x) #t)
)
(define body?
    (lambda (exp)
        (or ((list-of expression?) exp) (expression? exp))
    )
)


(define variable-args?
    (lambda (d) 
        (or 
            (null? d)
            (symbol? d)
            (and (pair? d) (not (list? d)))
        )
    )
)

(define closure-args?
    (lambda (d) 
        (or 
            (symbol? d)
            (and (pair? d) (not (list? d)))
            (list? d)
        )
    )
)

(define var-assign-list? (lambda (ls) (and (list? ls) (= 2 (length ls)) (symbol? (car ls)))))

(define parse-exp         
  (lambda (datum)
    (cond
        [(symbol? datum) (var-exp datum)]
        [(null? datum) (lit-exp datum)]
        [(list? datum) ; come back to this if something breaks!
            (cond
                [(eqv? (car datum) 'quote)
                    (lit-exp (cadr datum))
                ]
                [(eqv? (car datum) 'lambda)
                    (cond 
                        [(null? (cddr datum))
                            (eopl:error 'parse-exp "lambda-expression: missing body ~s" datum)
                        ]
                        [(list? (2nd datum)) ;checking for args
                            (if ((list-of symbol?) (2nd datum))
                                (if (null? (cddr datum)) ;no body
                                    (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)
                                    (if (list? (3rd datum))
                                        (lambda-body-is-list-exp 
                                            (2nd datum)  
                                            (if (null? (cdddr datum))
                                                (parse-exp (3rd datum))
                                                (letrec ([helper (lambda (x) (if (null? (cdr x)) (parse-exp (car x)) (helper (cdr x))))])
                                                    (helper (cddr datum)))
                                            )
                                        )
                                        (lambda-body-not-list-exp (2nd datum) (map parse-exp (cddr datum)))
                                    )  
                                )
                                (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" datum)
                            )
                        ]
                        [(or (symbol? (2nd datum)) (and (not (list? (2nd datum))) (pair? (2nd datum)))) ; variable args
                            (lambda-variable-args-exp (2nd datum) (map parse-exp (cddr datum)))
                        ]
                        [else (eopl:error 'parse-exp "lambda-expression: bad lambda expression: ~s" datum)
]
                    )    
            ]
            [(eqv? (car datum) 'if)
                (if (null? (cddr datum))
                    (eopl:error 'parse-exp "if expression: should have (only) test, then, and else clauses: ~s" datum)
                    (if (null? (cdddr datum))
                        (if-exp-no-just (parse-exp (2nd datum))
                            (parse-exp (3rd datum))
                        )
                        (if-exp (parse-exp (2nd datum))
                            (parse-exp (3rd datum))
                            (parse-exp (4th datum))
                        )
                    )
                )  
            ]
            [(eqv? (car datum) 'let)
                (cond 
                    [(null? (cddr datum))
                        (eopl:error 'parse-exp "Error in parse-expression: let expression: incorrect length: ~s" datum)
                    ]
                    [(or (not ((list-of pair?) (2nd datum))) (not (andmap var-assign-list? (2nd datum))))
                        (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list of pairs of length 2: ~s" datum)
                    ]
                    [(list? (2nd datum)) ; unnamed           
                        (let-exp (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (2nd datum))
                            (map parse-exp (cddr datum))
                        )
                    ]
                    [else 
                        (named-let-exp (2nd datum)
                            (3rd datum)
                            (map parse-exp (cddr datum))
                        )
                    ]
                ) 
            ]
            [(eqv? (car datum) 'let*)
                
                (cond 
                    [(or (not ((list-of pair?) (2nd datum))) (not (andmap var-assign-list? (2nd datum))))
                        (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list of pairs of length 2: ~s" datum)
                    ]
                    [(list? (2nd datum)) ; unnamed           
                        (let*-body-not-list-exp (map (lambda (x) (list (car x) (parse-exp (cadr x)))) (2nd datum))
                            (map parse-exp (cddr datum))
                        )
                    ]
                    ; [(list? (3rd datum))
                    ;     (let*-body-is-list-exp 
                    ;         (2nd datum)
                    ;         (parse-exp (3rd datum))
                    ;     )
                    ; ]
                    ; [else (let*-body-not-list-exp 
                    ;         (map parse-exp (2nd datum))
                    ;         (map parse-exp (cddr datum))
                    ;     )
                    ; ]

                )
            ]
            [(eqv? (car datum) 'letrec)
                (cond 
                    [(or (not ((list-of pair?) (2nd datum))) (not (andmap var-assign-list? (2nd datum))))
                        (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list of pairs of length 2: ~s" datum)
                    ]
                    [(null? (cddr datum)) 
                        (eopl:error 'parse-exp "Error in parse-expression: letrec expression: incorrect length: ~s" datum)
                    ]
                    [else 
                        (letrec-exp (map parse-exp (2nd datum))
                            (map parse-exp (cddr datum))
                        )
                    ]

                )
            ]
            [(eqv? (car datum) 'set!)
                (cond
                    [(null? (cddr datum))
                        (eopl:error 'parse-exp "Error in parse-expression: set!: missing expression:  ~s" datum)
                    ]
                    [(not (null? (cdddr datum)))
                        (eopl:error 'parse-exp "Error in parse-expression: set!: Too many parts:  ~s" datum)
                    ]
                    [else (set!-exp (2nd datum) (parse-exp (3rd datum)))]
                )    
            ]
            [(eqv? (car datum) 'cond)
                (cond-exp (cdr datum))
            ]
            [(eqv? (car datum) 'begin)
                (begin-exp (cdr datum))
            ]
            [(eqv? (car datum) 'case)
                (case-exp (2nd datum) (cddr datum))
            ]
            [(eqv? (car datum) 'while)
                (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))
            ]
            [else   
                (app-exp 
                    (parse-exp (1st datum)) 
                    (map parse-exp (cdr datum))
                )     
            ]
        )
        ]
        [(datum? datum) (lit-exp datum)]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)]

    )
  )
)


(define unparse-exp
    (lambda (exp)
        (cases expression exp
            [var-exp (id) id]
            [lit-exp (val) val]
            [lambda-body-is-list-exp (args body)
                (list 'lambda args (unparse-exp body))
            ]
            [lambda-body-not-list-exp (args body)
                (append (list 'lambda args) (map unparse-exp body))
            ]
            [lambda-variable-args-exp (args body)
                (append (list 'lambda) (list args) (map unparse-exp body))
            ]
            [if-exp-no-just (pred then_case)
                (list 'if pred (unparse-exp then_case))
            ]
            [if-exp (pred then_case just_in_case)
                (list 'if (unparse-exp pred) (unparse-exp then_case) (unparse-exp just_in_case))
            ]
            [let-exp (vars body)
                (append (list 'let vars) (map unparse-exp body))
            ]
            [named-let-exp (name vars body)
                (append (list 'let name vars) (map unparse-exp body))
            ]
            ; [let*-body-is-list-exp (vars body) 
            ;     (list 'let* vars (unparse-exp body))
            ; ]
            [let*-body-not-list-exp (vars body) 
                (append (list 'let* vars) (map unparse-exp body))
            ]
            [letrec-exp (vars body)
                (append (list 'letrec (map unparse-exp vars)) (map unparse-exp body))
            ]
            [set!-exp (var body)
                (list 'set! var (unparse-exp body))
            ]
            [app-exp (rator rands)
                (append (list (unparse-exp rator))
                (map unparse-exp rands))
            ]
            [cond-exp (bodies) (list 'cond (map unparse-exp bodies))]
        )
    )
)









