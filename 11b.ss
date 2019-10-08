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
        (id symbol?)
    ]
    [lit-exp
        (val number?)
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
    ; [if-exp-no-just
    ;     (pred expression?)
    ;     (then_case expression?)
    ; ]
    ; [if-exp
    ;     (pred expression?)
    ;     (then_case expression?)
    ;     (just_in_case expression?)
    ; ]
    [let-exp
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [let*-exp
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    [named-let-exp
        (name symbol?)
        (vars (list-of pair?))
        (body (list-of expression?))
    ]
    ; [letrec-exp
    ;     (vars (list-of pair?))
    ;     (body (list-of expression?))
    ; ]
)

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
        [(eqv? (car datum) 'lambda)
            (cond 
                [(list? (2nd datum))
                    (if (list? (3rd datum))
                        (lambda-body-is-list-exp (2nd datum) (parse-exp (3rd datum)))
                        (lambda-body-not-list-exp (2nd datum) (map parse-exp (cddr datum)))
                    )  
                ]
                [else (lambda-no-args-exp (map parse-exp (cdr datum)))]
            )
            
        ]
        ; [(eqv? (car datum) 'if)
        ;     (let ([len (length datum)])
        ;         (cond 
        ;             [(= len 3) 
        ;                 (if-exp (2nd datum) 
        ;                     (parse-exp (3rd datum))
        ;                 )
        ;             ]
        ;             [(= len 4) 
        ;                 (if-exp (2nd datum) 
        ;                     (parse-exp (3rd datum))
        ;                     (parse-exp (4th datum))
        ;                 )
        ;             ]
        ;         )
        ;     )  
        ; ]
        [(eqv? (car datum) 'let)
            (let ([len (length datum)])
                (cond 
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
            )  
        ]
        [(eqv? (car datum) 'let*)
            (let*-exp (2nd datum) 
                (parse-exp (3rd datum))
            )
        ]
        ; [(eqv? (car datum) 'letrec)
        ;     (letrec-exp (2nd datum)
        ;         (parse-exp (3rd datum))
        ;     )
        ; ]
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
                        (parse-exp (2nd datum))
                    )
                ]
            )
        ]
      )
     ]
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
            ; [if-exp-no-just (pred then_case)
            ;     (list 'if pred (unparse-exp then_case))
            ; ]
            ; [if-exp (pred then_case just_in_case)
            ;     (list 'if pred (unparse-exp then_case) (unparse-exp just_in_case))
            ; ]
            [let-exp (vars body)
                (append (list 'let vars) (map unparse-exp body))
            ]
            [named-let-exp (name vars body)
                (append (list 'let name vars) (map unparse-exp body))
            ]
            [let*-exp (vars body)
                (list 'let* vars (unparse-exp body))
            ]
            ; [letrec-exp (vars body)
            ;     (list 'letrec vars (unparse-exp body))
            ; ]
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








