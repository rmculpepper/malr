#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/datum syntax/transformer))
(provide (all-defined-out))

;; (hash-from-definitions Body ...) : Expr
(define-syntax hash-from-definitions
  (syntax-parser
    [(_ e:expr ...)
     #:with HERE (datum->syntax this-syntax 'HERE)
     #'(let ([h (make-hash)])
         (convert-defs e h HERE) ...
         (#%expression h))]))

;; (convert-defs Body SimpleExpr[MutableHash] LCtx) : Body
(define-syntax convert-defs
  (syntax-parser
    [(_ e:expr h here)
     (define ee (local-expand #'e (syntax-local-context) #f))
     (syntax-parse ee
       #:literal-sets (kernel-literals)
       [(define-values ~! (x:id ...) rhs:expr)
        #:with (tmp ...) (generate-temporaries #'(x ...))
        #'(begin (define-values (tmp ...) rhs)
                 (define-key-maybe x tmp h here) ...)]
       [(begin ~! e:expr ...)
        #'(begin (convert-defs e h here) ...)]
       [ee
        #'ee])]))

;; (define-key-maybe x:Id Expr Expr[MutableHash] LCtx) : Body[{x}]
(define-syntax define-key-maybe
  (syntax-parser
    [(_ x:id tmp:id h:expr here)
     (cond [(bound-identifier=? #'x (datum->syntax #'here (syntax-e #'x)))
            #'(begin (define-syntax x
                       (make-variable-like-transformer
                        (quote-syntax (hash-ref h (quote x)))
                        (quote-syntax (lambda (v) (hash-set! h (quote x) v)))))
                     (set! x tmp))]
           [else
            #'(define-syntax x (make-rename-transformer (quote-syntax tmp)))])]))

;; ----------------------------------------

(require rackunit)

(define h1
  (hash-from-definitions
    (define x 1)
    (define y (+ x x))
    (define (x-up!) (set! x (+ x y)))
    (define (xy) (* x y))))
(check-equal? (hash-ref h1 'x) 1)
(check-equal? ((hash-ref h1 'xy)) 2)
((hash-ref h1 'x-up!))
(check-equal? (hash-ref h1 'x) 3)
(check-equal? ((hash-ref h1 'xy)) 6)

;; Problem:

(define-syntax define/get
  (syntax-parser
    [(_ var:id rhs:expr)
     #'(begin (define tmp rhs)
              (define (var) tmp))]))
(define h2
  (hash-from-definitions
    (define/get x 1)
    (define/get y (+ (x) (x)))))
(check-equal? ((hash-ref h2 'x)) 1) ;; XFAIL
(check-equal? ((hash-ref h2 'y)) 2)
