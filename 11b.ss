; Carson Thrift and Olivia Penry
; 10/8/19
; Assignment 11b
; CSSE304

; Problem 4

; (load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

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

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define datum?
    (lambda (d)
        (or (null? d)
            (number? d) 
            (symbol? d) 
            (vector? d)
            (boolean? d)
            (char? d)
            ((list-of datum?) d) ; Is this right for quoted lists?
        )
    )
)
(define 2-list? (lambda (ls) (= 2 (length ls))))

(define parse-exp         
  (lambda (datum)
    (cond
        [(symbol? datum) (var-exp datum)]
        [(and (list? datum) (not (null? (cdr datum))))
            (cond
                [(eqv? (car datum) 'lambda)
                    (cond 
                        [(null? (cddr datum))
                            (eopl:error 'parse-exp "lambda-expression: missing body ~s" datum)
                        ]
                        [(list? (2nd datum))
                            (if ((list-of symbol?) (2nd datum))
                                (if (null? (cddr datum))
                                    (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)
                                    (if (list? (3rd datum))
                                        (lambda-body-is-list-exp (2nd datum) (parse-exp (3rd datum)))
                                        (lambda-body-not-list-exp (2nd datum) (map parse-exp (cddr datum)))
                                    )  
                                )
                                (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" datum)
                            )
                        ]
                        [else (lambda-no-args-exp (map parse-exp (cdr datum)))]
                    )    
            ]
            [(eqv? (car datum) 'if)
                (if (null? (cddr datum))
                    (eopl:error 'parse-exp "if expression: should have (only) test, then, and else clauses: ~s" datum)
                    (if-exp (parse-exp (2nd datum))
                        (parse-exp (3rd datum))
                        (parse-exp (4th datum))
                    )
                )
                ; (let ([len (length datum)])
                ;     (cond 
                ;         [(= len 3) 
                ;             (if-exp (2nd datum) 
                ;                 (parse-exp (3rd datum))
                ;             )
                ;         ]
                ;         [(= len 4) 
                ;             (if-exp (2nd datum) 
                ;                 (parse-exp (3rd datum))
                ;                 (parse-exp (4th datum))
                ;             )
                ;         ]
                ;     )
                ; )  
            ]
            [(eqv? (car datum) 'let)
                (cond 
                    [(null? (cddr datum))
                        (eopl:error 'parse-exp "Error in parse-expression: let expression: incorrect length: ~s" datum)
                    ]
                    [(or (not ((list-of pair?) (2nd datum))) (not (andmap list? (2nd datum))) (not (andmap 2-list? (2nd datum))))
                        (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list of pairs of length 2: ~s" datum)

                    ]
                    [(list? (2nd datum)) ; unnamed           
                        (let-exp (2nd datum) 
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
                    [(or (not ((list-of pair?) (2nd datum))) (not (andmap list? (2nd datum))) (not (andmap 2-list? (2nd datum))))
                        (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list of pairs of length 2: ~s" datum)
                    ]
                    [(list? (3rd datum))
                        (let*-body-is-list-exp (2nd datum) 
                            (parse-exp (3rd datum))
                        )
                    ]
                    [else (let*-body-not-list-exp (2nd datum) 
                            (map parse-exp (cddr datum))
                        )
                    ]

                )
            ]
            [(eqv? (car datum) 'letrec)
                (cond 
                    [(or (not ((list-of pair?) (2nd datum))) (not (andmap list? (2nd datum))) (not (andmap 2-list? (2nd datum))))
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
                (set!-exp (2nd datum) (parse-exp (3rd datum)))
            ]
            [else   
                (cond 
                    [(> (length datum) 2)
                        (app-exp 
                            (parse-exp (1st datum)) 
                            (map parse-exp (cdr datum))
                        )
                    ]
                    [else 
                        (app-exp (parse-exp (1st datum))
                            (list (parse-exp (2nd datum)))
                        )
                    ]
                )
            ]
        )
        ]
        [(datum? datum) (lit-exp datum)]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


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
            [lambda-no-args-exp (body)
                (append (list 'lambda) (map unparse-exp body))
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
            [let*-body-is-list-exp (vars body) 
                (list 'let* vars (unparse-exp body))
            ]
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
        )
    )
)


; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
     
; (var-exp? (var-exp 'a))
; (var-exp? (app-exp (var-exp 'a) (var-exp 'b)))








