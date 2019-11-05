
(define *prim-proc-names* '(+ - * / add1 sub1 zero? not append cons car cdr caar cadr cdar cddr 
                              caaar caadr cadar cdaar cddar cdadr caddr cdddr list null? assq eq? eqv? equal? atom? length 
                               list->vector list-tail list? pair? procedure? vector->list vector make-vector vector-ref vector? number? 
                                symbol? set-car! set-cdr! vector-set! display newline = < > <= >= quote apply map void and memv quotient))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define global-env init-env)

(define reset-global-env
  (lambda () (set! global-env init-env))
)

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (cases expression form
      [define-exp (name definition) 
        (set! global-env (extend-env (list name) (list (eval-exp definition (empty-env))) global-env))
      ]
      [else (eval-exp form (empty-env))]
    )
  )
)

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp local-env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env local-env id; look up its value.
      	  (lambda (x) x) ; procedure to call if id is in the environment 
          (lambda ()  
            (apply-env-ref global-env id
              (lambda (x) x)
              (lambda () 
                (eopl:error 'apply-env
                "variable ~s is not bound"
                id)))))] 
          ;(eopl:error 'apply-env ; procedure to call if id not in env
		      ;  "variable not found in environment: ~s"
			    ;  id)
          
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator local-env)]
              [args (if (equal? rator (var-exp 'quote)) ; special case for quote
                        (map unparse-exp rands)
                        ; (eval-rands rands local-env)
                        (cases proc-val (eval-exp rator local-env) ; THE PROBLEM IS HERE :() MAGIKARP, I CHOOSE YOUUUUUU
                          [closure (args body local-env) (eval-rands-ref args rands local-env)]
                          [else (eval-rands rands local-env)]
                        )
                    )
              ])
              (apply-proc proc-value args)
          )
      ]
      [lambda-exp (args body) 
        (closure args body local-env)
      ]
      [lambda-variable-args-exp (args body)
        (closure args body local-env)
      ]
      [if-exp (pred then_case just_in_case)
        (if (eval-exp pred local-env)
          (eval-exp then_case local-env)
          (eval-exp just_in_case local-env)
        )
      ]
      [if-exp-no-just (pred then_case)
        (if (eval-exp pred local-env)
          (eval-exp then_case local-env)
          (void)
        )
      ]
      [while-exp (test-exp bodies)
        (if (eval-exp test-exp local-env) 
          (begin
            (eval-bodies bodies local-env)
            (eval-exp exp local-env)
          )
        )
      ]
      [set!-exp (id exp)
        (set-ref! 
          (apply-env-ref local-env id 
            (lambda (x) x) ; procedure to call if id is in the environment 
            (lambda () 
              (apply-env-ref global-env id
                (lambda (x) x)
                (lambda () 
                  (eopl:error 'apply-env
                  "in set!-exp, variable ~s is not bound"
                  id))
              )
            )
          )
          (eval-exp exp local-env)
        )
        
      ]
      [define-exp (name defintion) (top-level-eval exp)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands local-env)
    (map (lambda (x) (eval-exp x local-env)) rands)
  )
)


(define eval-rands-ref
  (lambda (args-of-closure rands local-env)
    (if (null? rands) 
      '()
      ; (display local-env)
      (if (symbol? (car args-of-closure))
        (cons 
          (eval-exp (car rands) local-env) 
          (eval-rands-ref (cdr args-of-closure) (cdr rands) local-env)
        )
        (cons 
          (apply-env-ref local-env (car args-of-closure)
            (lambda (x) x)
            (lambda () 
                    (eopl:error 'eval-rands-ref
                    "called apply-env-ref and ith a bith: ~s"
                    (car args-of-closure)))) 
          (eval-rands-ref (cdr args-of-closure) (cdr rands) local-env)
        )
      ) 
    )
  )
)

(define eval-bodies
  (lambda (bodies local-env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) local-env)
      (begin
        (eval-exp (car bodies) local-env)
        (eval-bodies (cdr bodies) local-env)
      )
    )
  )
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (arg-names bodies local-env) 
        (cond 
          [(symbol? arg-names) 
            (eval-bodies bodies (extend-env (list arg-names) (list args) local-env))
          ]
          [(list? arg-names)
            (eval-bodies bodies (extend-env arg-names args local-env))
          ]
          [else 
            (eval-bodies 
              bodies 
              (extend-env 
                (flatten arg-names) 
                (let loop ([arg-names arg-names] [ls args]) 
                  (if (symbol? (cdr arg-names))
                    (list (car ls) (cdr ls))
                    (cons (car ls) (loop (cdr arg-names) (cdr ls)))
                  )
                )
                local-env)
            )
          ]
        )
      ]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define flatten
  (lambda (iL)
    (if (null? iL) 
      '()
      (if (symbol? (cdr iL))
        (list (car iL) (cdr iL))
        (cons (car iL) (flatten (cdr iL)))
      )
    )
  )
)


; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (quotient (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(append) (apply append args)]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (car (1st args))] 
      [(cdr) (cdr (1st args))] 
      [(caar) (caar (1st args))] 
      [(cadr) (cadr (1st args))] 
      [(cdar) (cdar (1st args))] 
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(list) args] 
      [(null?) (null? (1st args))] 
      [(assq) (assq (1st args) (2nd args))] 
      [(eq?) (eq? (1st args) (2nd args))] 
      [(eqv?) (eqv? (1st args) (2nd args))] 
      [(equal?) (equal? (1st args) (2nd args))] 
      [(atom?) (atom? (1st args))] 
      [(length) (length (1st args))] 
      [(list->vector) (list->vector (1st args))] 
      [(list-tail) (list-tail (1st args) (2nd args))]
      [(list?) (list? (1st args))] 
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (if (list? args) (1st args) args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (make-vector (1st args) (2nd args))
        (cond 
          [(null? (2nd args))
            (make-vector (1st args))
          ]
          [else (make-vector (1st args) (2nd args))]
        )
      ]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display args)]
      [(newline) (newline)]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(quote) (1st args)]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(map) 
        (let ([p (1st args)])
          (map (lambda (x) (apply-proc p (list x))) (2nd args))
        )
      ]
      [(void) (void)]
      [(memv) (memv (1st args) (2nd args))]
      [(and) (andmap (lambda (x) (and #t x)) args)]
      ; [(or) (ormap (lambda (x) (or x #f)) args)]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))


(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

(define syntax-expand 
  (lambda (exp)
    (cases expression exp
      [let-exp (vars body)
        (app-exp (lambda-exp (map car vars) (map syntax-expand body)) (map syntax-expand (map cadr vars))) ;returns an equivalent application expression from the original parsed let expression
      ]
      [let*-body-not-list-exp (vars body) 
        (if (null? (cdr vars))
          (syntax-expand (let-exp vars body))
          (syntax-expand (let-exp (list (car vars)) (list (syntax-expand (let*-body-not-list-exp (cdr vars) body)))))
        )
      ]
      [cond-exp (bodies) 
        (if (null? bodies)
          (app-exp (var-exp 'void) '())
          (if (eq? (caar bodies) 'else)
            (parse-exp (cadar bodies))
            (if-exp (syntax-expand (parse-exp (caar bodies))) (parse-exp (cadar bodies)) (syntax-expand (cond-exp (cdr bodies))))
          )
        )
      ]
      [begin-exp (bodies)
        (app-exp (lambda-exp '() (map syntax-expand bodies)) '())
      ]
      [case-exp (condition bodies)
        (syntax-expand (cond-exp
          (letrec ([ow-owwww! 
            (lambda (conditioner bodayeez) 
                (if (eq? (caar bodayeez) 'else)
                  (list (cons 'else (cdar bodayeez)))
                  (cons (cons (list 'memv conditioner (list 'quote (caar bodayeez))) (cdar bodayeez))
                        (ow-owwww! conditioner (cdr bodayeez))
                  )
                )
            )])
            (ow-owwww! condition bodies)
          )
        ))
      ]
      [app-exp (rator rands) (app-exp (syntax-expand rator) (map syntax-expand rands))]
      [lambda-exp (args body) (lambda-exp args (map syntax-expand body))]
      [if-exp (pred then_case just_in_case) (if-exp (syntax-expand pred) (syntax-expand then_case) (syntax-expand just_in_case))]
      [if-exp-no-just (pred then_case) (if-exp-no-just (syntax-expand pred) (syntax-expand then_case))]
      [var-exp (var) exp]
      [lit-exp (val) exp]
      [letrec-exp (proc-names idss bodiess letrec-bodies) 
        (syntax-expand 
          (let-exp 
            (build-vars proc-names)
            (list (begin-exp (append (build-bodies proc-names idss (map (lambda (x) (map syntax-expand x)) bodiess)) (map syntax-expand letrec-bodies))))
          )
        )      ]
      [define-exp (name definition) (define-exp name (syntax-expand definition))]
      [or-exp (bodies) 
        (syntax-expand (letrec ([helper-barely-know-her?!?
          (lambda (bodies)
            (if (null? bodies)
              (lit-exp #f)
              (let-exp (list (list 'benq (car bodies))) 
                (list (if-exp (var-exp 'benq) (var-exp 'benq) (helper-barely-know-her?!? (cdr bodies))))
              )
            )          
          )
          
          ]) 
        (helper-barely-know-her?!? bodies)))
      ]
        ; fill in all others
      [else exp]
    )
  )
)

(define build-vars
  (lambda (proc-names)
    (if (null? proc-names)
      '()
      (cons (list (car proc-names) (lit-exp 3)) (build-vars (cdr proc-names)))
    )
  )
)

(define build-bodies 
  (lambda (proc-names idss bodiess)
    (if (null? proc-names)
      '()
      (cons (set!-exp (car proc-names) (lambda-exp (car idss) (car bodiess))) (build-bodies (cdr proc-names) (cdr idss) (cdr bodiess)))
    )
  )
)
