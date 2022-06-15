#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/datum
                     syntax/transformer syntax/flatten-begin)
         racket/contract)
(provide (all-defined-out))

;; ----------------------------------------

(begin-for-syntax
  ;; generic-pass1 : (Listof Syntax[Body]) (Listof Id)
  ;;                 [#:ctx ExpandContext]
  ;;                 [#:defctx DefContext]
  ;;          -> (Listof Syntax[Body]) (Listof Id) (Listof Id)
  (define (generic-pass1 bs stops
                         #:defctx [defctx (syntax-local-make-definition-context)]
                         #:ctx [ectx (list (gensym))])
    (define stops* (append stops (list #'define-values #'define-syntaxes)))
    (define-syntax-class stop-id
      (pattern x:id #:when (member #'x stops* free-identifier=?)))
    (let loop ([bs bs] [acc null])
      (cond [(pair? bs)
             (define eb (local-expand (car bs) ectx stops* defctx))
             (syntax-parse eb
               #:literal-sets (kernel-literals)
               [(begin ~! b:expr ...)
                (loop (append (flatten-begin eb) (cdr bs)) acc)]
               [(define-values ~! (x:id ...) rhs:expr)
                (syntax-local-bind-syntaxes (datum (x ...)) #f defctx)
                (loop (cdr bs) (cons eb acc))]
               [(define-syntaxes ~! (x:id ...) rhs:expr)
                (syntax-local-bind-syntaxes (datum (x ...)) #'rhs defctx)
                (loop (cdr bs) (cons eb acc))]
               [(s:stop-id ~! . _)
                (loop (cdr bs) (cons eb acc))]
               [_:expr
                (loop (cdr bs) (cons eb acc))])]
            [else (reverse acc)]))))

;; FIXME: hybrid technique, use trampoline for define-syntaxes
;; If (define-syntaxes (x ...) (values)) trick were allowed in internal positions,
;; that would be better (scoping).
