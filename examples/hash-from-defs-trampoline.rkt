#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/datum syntax/transformer))
(provide (all-defined-out))

;; (hash-from-definitions Body ...) : Expr
(define-syntax hash-from-definitions
  (syntax-parser
    [(_ body ...)
     #'(let ([h (make-hash)])
         (convert-defs body h) ...
         h)]))

;; (convert-defs Body SimpleExpr[MutableHash]) : Body
(define-syntax convert-defs
  (syntax-parser
    [(_ e:expr h)
     (define ee (local-expand #'e (syntax-local-context) #f))
     (syntax-parse ee
       #:literal-sets (kernel-literals)
       [(define-values ~! (x:id ...) rhs:expr)
        (with-syntax ([(tmp ...) (generate-temporaries #'(x ...))])
          #'(begin (define-values (tmp ...) rhs)
                   (define-key x tmp h) ...))]
       [(begin ~! e:expr ...)
        #'(begin (convert-defs e h) ...)]
       [ee
        #'ee])]))

;; (define-key x:Id Expr Expr[MutableHash]) : Body[{x}]
(define-syntax define-key
  (syntax-parser
    [(_ x:id tmp:id h:expr)
     #'(begin (define-syntax x
                (make-variable-like-transformer
                  (quote-syntax (hash-ref h (quote x)))
                  (quote-syntax (lambda (v) (hash-set! h (quote x) v)))))
              (set! x tmp))]))


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
(test-case "XFAIL"
  (check-equal? ((hash-ref h2 'x)) 1))
(check-equal? ((hash-ref h2 'y)) 2)
