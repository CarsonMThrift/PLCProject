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
            (symbol? d) 
            (vector? d)
            (boolean? d)
            (char? d)
            ((list-of datum?) d) ; Is this right for quoted lists?
        )
    )
)
(define var-assign-list? (lambda (ls) (and (list? ls) (= 2 (length ls)) (symbol? (car ls)))))

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
                        (let-exp (map parse-exp (2nd datum))
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
                    [(list? (3rd datum))
                        (let*-body-is-list-exp 
                            (map parse-exp (2nd datum)) 
                            (parse-exp (3rd datum))
                        )
                    ]
                    [else (let*-body-not-list-exp 
                            (map parse-exp (2nd datum))
                            (map parse-exp (cddr datum))
                        )
                    ]

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
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)]

    )
  )
)












