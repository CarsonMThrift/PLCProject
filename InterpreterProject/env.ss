; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map (lambda (x) (if (box? x) x (box x))) vals) env)))

; (define extend-env-recursively
;   (lambda (proc-names idss bodiess old-env)
;     (recursively-extended-env-record proc-names idss bodiess old-env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env 
  (lambda (env var succeed fail)
    ; (display env)
    (deref (apply-env-ref env var succeed fail)) ; pass in funcs mentioned below?
  )
)

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are "callback procedures, 
    (cases environment env       ;  succeed is appluied if sym is found, otherwise 
      [empty-env-record ()       ;  fail is applied.
        (fail)] ; fail could be an error saying the sym wasn't found
      [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
            (if (number? pos)
              (succeed (list-ref vals pos)) ; succeed could just return the list-ref vals pos
              (apply-env-ref env sym succeed fail)))]
      ; [recursively-extended-env-record (procnames idss bodiess old-env)
      ;   (let ([pos (list-find-position sym procnames)])
      ;     (if (number? pos)
      ;       (box (closure (list-ref idss pos)
      ;         (list-ref bodiess pos)
      ;         env))
      ;       (apply-env-ref old-env sym succeed fail)))]
    )
  )
)


