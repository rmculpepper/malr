#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base racket/match))

@(define the-eval (make-base-eval))

@title[#:tag "solutions" #:style '(toc)]{Solutions to Selected Exercises}

@(local-table-of-contents)

@;{Each solution gets a separate section and a separate page, to
prevent spoilers.}

@;{
@; ----------------------------------------
@solution-section[#:tag "noisy-v1"]
@racketblock[
(define-syntax-rule (noisy-v1 expr)
  (begin (printf "evaluating ~s\n" 'expr) expr))
]
}

@;{
@; ----------------------------------------
@solution-section[#:tag "noisy-v2"]
@racketblock[
(define-syntax-rule (noisy-v2 expr)
  (begin (printf "evaluating ~s..." 'expr) 
         (begin0 expr (printf "done\n"))))
]
}

@; ----------------------------------------
@solution-section[#:tag "iflet"]

@racketblock[
(define-syntax-rule (iflet x e1 e2 e3)
  (let ([tmp e1])
    (if tmp
        (let ([x tmp]) e2)
        e3)))
]

The following version is wrong, because it puts @racket[e3] in the
scope of @racket[x]:

@racketblock[
(code:comment "WRONG")
(define-syntax-rule (iflet x e1 e2 e3)
  (let ([x e1])
    (if x e2 e3)))
]

An alternative is to ``freeze'' @racket[e3] outside of the
@racket[let]-binding of @racket[x] and then ``thaw'' it inside:

@racketblock[
(code:comment "OKAY")
(define-syntax-rule (iflet x e1 e2 e3)
  (let ([thunk3 (lambda () e3)])
    (let ([x e1])
      (if x e2 (thunk3)))))
]

Don't be afraid to use @racket[lambda] to control the environment and
evaluation order of an expression. In code such as this, the Racket
compiler can inline the call to @racket[thunk3]; that @racket[lambda]
costs nothing at run time.


@; ----------------------------------------
@solution-section[#:tag "andlet1-w-fun"]

When implementing a macro's dynamic behavior with a function, an
expression becomes a function; an expression to be evaluated in an
extended environment becomes a function with arguments.

In @racket[andlet1], @racket[e2] is evaluated in the scope of the
variable @racket[x]. So the function for @racket[e2] has a single
formal parameter: @racket[x].

@racketblock[
(define-syntax-rule (andlet1 x e1 e2)
  (andlet1-fun (lambda () e1) (lambda (x) e2)))

(define (andlet1-fun thunk1 fun2)
  (let ([tmp (thunk1)])
    (if tmp
        (fun2 tmp)
        #f)))
]

Of course, this technique breaks down if the variable's scope includes
multiple expressions handled separately by the macro and if the
expressions use @racket[set!] to mutate the variable.


@; ----------------------------------------
@solution-section[#:tag "my-cond-v0"]

An expression is represented by a function; a clause containing two
expressions can be represented by a pair of functions.

@racketblock[
(define-syntax-rule (my-cond-v0 [question-expr answer-expr] ...)
  (my-cond-v0-fun
   (list
    (cons    (code:comment "bleh, I know, improper use of cons")
     (lambda () question-expr)
     (lambda () answer-expr))
    ...)))

(define (my-cond-v0-fun clauses)
  (if (pair? clauses)
      (let ([question-thunk (car (car clauses))]
            [answer-thunk (cdr (car clauses))])
        (if (question-thunk)
            (answer-thunk)
            (my-cond-v0-fun (cdr clauses))))
      (void)))
]


@; ----------------------------------------
@solution-section[#:tag "minimatch1"]

The main macro, @racket[minimatch1], just evaluates the expression to
match and stores it in a private variable. The helper macro,
@racket[minimatch1*], does all of the work.

@racketblock[
(define-syntax-rule (minimatch1 val-expr pattern result-expr)
  (let ([v val-expr])
    (minimatch1* v pattern result-expr)))
]

We have no way (yet) of writing a pattern that recognizes just
identifiers; so we put the variable case last. Otherwise, the
``variable'' case would match the other two kinds of patterns as well,
resulting in syntax errors from @racket[let].

@racketblock[
(define-syntax minimatch1*
  (syntax-rules (cons quote)
    [(minimatch1* v-pv (@#,racket[quote] datum) result-expr)
     (if (equal? v-pv (@#,racket[quote] datum))
         result-expr
         (error 'minimatch1 "match failed"))]
    [(minimatch1* v-pv (cons first-pattern rest-pattern) result-expr)
     (if (pair? v-pv)
         (let ([first-var (car v-pv)]
               [rest-var (cdr v-pv)])
           (minimatch1* first-var first-pattern
                        (minimatch1* rest-var rest-pattern result-expr)))
         (error 'minimatch1 "match failed"))]
    [(minimatch1* v-pv variable-id result-expr)
     (let ([variable-id v-pv])
       result-expr)]))
]

@;{Subtlety: consider pattern (cons cons (cons a b)).}


@; ----------------------------------------
@solution-section[#:tag "minimatch"]

Like @racket[minimatch1], the @racket[minimatch] macro needs a private
variable for the value being matched. It needs a second private
variable to represent the failure action. Consider the pattern
@racket[(cons 'a 'b)]. The whole pattern can fail, because the value
is not a pair; the first sub-pattern can fail; and the second
sub-pattern can fail. There must be three occurrences of the same
failure action---thus to avoid bad duplication, we must introduce a
private variable.

The main macro, @racket[minimatch], just sets up the private variable
for the value to be matched. The first helper macro,
@racket[minimatch-clauses], recurs through the clauses and sets up
private variables for failure actions. The second helper macro is
similar to the helper macro for @racket[minimatch1], but it takes an
extra argument, a failure expression, that it uses instead of directly
calling @racket[error].

@racketblock[
(define-syntax minimatch
  (syntax-rules ()
    [(minimatch val-expr clause ...)
     (let ([v val-expr])
       (minimatch-clauses v clause ...))]))

(define-syntax minimatch-clauses
  (syntax-rules ()
    [(minimatch-clauses v)
     (error 'minimatch "match failed")]
    [(minimatch-clauses v [pattern1 result-expr1] clause ...)
     (let ([fail (lambda () (minimatch* v clause ...))])
       (minimatch1/fail v pattern1 result-expr1 (fail)))]))

(define-syntax minimatch1/fail
  (syntax-rules (cons quote)
    [(minimatch1/fail v (@#,racket[quote] datum) result-expr fail-expr)
     (if (equal? v (@#,racket[quote] datum))
         result-expr
         fail-expr)]
    [(minimatch1/fail v (cons first-pattern rest-pattern) result-expr fail-expr)
     (if (pair? v)
         (let ([first-var (car v)]
               [rest-var (cdr v)])
           (minimatch1/fail
             first-var 
             first-pattern
             (minimatch1/fail rest-var rest-pattern result-expr fail-expr)
             fail-expr))
         fail-expr)]
    [(minimatch1/fail v variable-id result-expr fail-expr)
     (let ([variable-id v])
       result-expr)]))
]


@; ----------------------------------------

@(close-eval the-eval)
