; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env init-env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator)]
              [args (eval-rands rands)])
          (apply-proc proc-value args 'null))]
      [lambda-body-not-list-exp (args body) 
        (apply-proc lambda-body-not-list-proc args body)
      ]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args body)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [lambda-body-not-list-proc (op) (lambda (args) body)]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not cons car cdr caar cadr cdar cddr 
                              caaar caadr cadar cdaar cddar cdadr caddr cdddr list null? assq eq? equal? atom? length 
                               list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? 
                                symbol? set-car! set-cdr! vector-set! display newline = < > <= >=))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (+ (1st args) (2nd args))]
      [(-) (- (1st args) (2nd args))]
      [(*) (* (1st args) (2nd args))]
      [(/) (/ (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
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
      [(null?) (null? args)] 
      [(assq) (assq (1st args) (2nd args))] 
      [(eq?) (eq? (1st args) (2nd args))] 
      [(equal?) (equal? (1st args) (2nd args))] 
      [(atom?) (atom? (1st args))] 
      [(length) (length args)] 
      [(list->vector) (list->vector args)] 
      [(list?) (list? args)] 
      [(pair?) (pair? args)]
      [(procedure?) (procedure? args)]
      [(vector->list) (vector->list args)]
      [(vector) (vector args)]
      [(make-vector) (make-vector (1st args) (2nd args))
        (cond 
          [(null? (2nd args))
            (make-vector (1st args))
          ]
          [else (make-vector (1st args) (2nd args))]
        )
      ]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? args)]
      [(number?) (number? args)]
      [(symbol?) (symbol? args)]
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
  (lambda (x) (top-level-eval (parse-exp x))))









