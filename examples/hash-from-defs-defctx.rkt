#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/datum
                     syntax/transformer syntax/flatten-begin)
         racket/contract)
(provide (all-defined-out))

;; ----------------------------------------

(define-syntax declare-key
  (lambda (stx) (raise-syntax-error #f "used out of context" stx)))

(begin-for-syntax
  (define-syntax-class header
    #:attributes (name) ;; Id
    (pattern name:id)
    (pattern (:header . _))))

(define-syntax define/key
  (syntax-parser
    [(_ h:header rhs:expr)
     #'(begin (declare-key h.name)
              (define h rhs))]))

;; ----------------------------------------

(begin-for-syntax
  ;; hfd-pass1 : (Listof Syntax[Body]) DefContext
  ;;          -> (Listof Syntax[Body]) (Listof Id) (Listof Id)
  (define (hfd-pass1 bs defctx)
    (define ectx (list (gensym)))
    (define stops (list #'declare-key #'define-values))
    (let loop ([bs bs] [acc null] [rvars null] [rkeys null])
      (cond [(pair? bs)
             (define eb (local-expand (car bs) ectx stops defctx))
             (syntax-parse eb
               #:literal-sets (kernel-literals)
               [(begin ~! b:expr ...)
                (loop (append (flatten-begin eb) (cdr bs)) acc rvars rkeys)]
               [(define-values ~! (x:id ...) rhs:expr)
                (syntax-local-bind-syntaxes (datum (x ...)) #f defctx)
                (define rvars* (append (reverse (datum (x ...))) rvars))
                (loop (cdr bs) (cons eb acc) rvars* rkeys)]
               [(define-syntaxes ~! (x:id ...) rhs:expr)
                (syntax-local-bind-syntaxes (datum (x ...)) #'rhs defctx)
                (loop (cdr bs) (cons eb acc) rvars rkeys)]
               [(declare-key ~! key:id ...)
                (define rkeys* (append (reverse (datum (key ...))) rkeys))
                (loop (cdr bs) acc rvars rkeys*)]
               [_:expr
                (loop (cdr bs) (cons eb acc) rvars rkeys)])]
            [else (values (reverse acc) (reverse rvars) (reverse rkeys))])))

  ;; mk-rewrite-defs : (Listof Id) Id Id -> Syntax[PartialExpandedBody] -> Syntax[Body]
  (define ((mk-rewrite-defs keys h-id origin-id) eb)
    (define (key? id) (member id keys bound-identifier=?))
    (syntax-parse eb
      #:literal-sets (kernel-literals)
      [(define-values ~! (x:id ...) rhs:expr)
       #:with ((bind-id def ...) ...)
       (for/list ([x (in-list (datum (x ...)))])
         (cond [(key? x)
                (define tmp (car (generate-temporaries (list x))))
                (list tmp
                      (with-syntax ([x x] [tmp tmp] [h h-id])
                        #'(begin (define-syntax x
                                   (make-variable-like-transformer
                                    (quote-syntax (hash-ref h (quote x)))
                                    (quote-syntax (lambda (v) (hash-set! h (quote x) v)))))
                                 (set! x tmp))))]
               [else (list x)]))
       ;; FIXME: track origin?
       (syntax-track-origin
        #'(begin (define-values (bind-id ...) rhs)
                 def ... ...)
        eb origin-id)]
      [_ eb])))

(define-syntax hash-from-definitions
  (syntax-parser
    [(hfd b:expr ...)
     (define defctx (syntax-local-make-definition-context))
     (define-values (ebs vars keys) (hfd-pass1 (datum (b ...)) defctx))
     (let ([dup (check-duplicate-identifier keys)])
       (when dup (raise-syntax-error #f "duplicate key declaration" dup)))
     (for ([key (in-list keys)])
       (unless (member key vars bound-identifier=?)
         (raise-syntax-error #f "key identifier not defined as variable" key)))
     (with-syntax ([(b* ...) (map (mk-rewrite-defs keys #'h #'hfd) ebs)])
       #'(let ([h (make-hash)])
           b* ...
           h))]))

;; ----------------------------------------

#;
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

;; ----------------------------------------

(require rackunit)

(define h1
  (hash-from-definitions
    (define/key x 1)
    (define/key y (+ x x))
    (define/key (x-up!) (set! x (+ x y)))
    (define/key (xy) (* x y))))
(check-equal? (hash-ref h1 'x) 1)
(check-equal? ((hash-ref h1 'xy)) 2)
((hash-ref h1 'x-up!))
(check-equal? (hash-ref h1 'x) 3)
(check-equal? ((hash-ref h1 'xy)) 6)

;; ----

(define-syntax define/get
  (syntax-parser
    [(_ var:id rhs:expr)
     #'(begin (define tmp rhs)
              (define (var) tmp))]))
(define h2
  (hash-from-definitions
   (declare-key x y)
   (define/get x 1)
   (define/get y (+ (x) (x)))))
(check-equal? ((hash-ref h2 'x)) 1)
(check-equal? ((hash-ref h2 'y)) 2)
