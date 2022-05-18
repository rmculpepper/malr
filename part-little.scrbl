;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/port rackunit))

@(define the-eval (make-malr-eval))
@(the-eval '(require (for-syntax racket/match)))

@; ============================================================
@title[#:tag "little-languages" #:version ""]{Little Languages}


@; ------------------------------------------------------------
@section[#:tag "datum-shape"]{The Datum Shape}

The @shape{Datum} shape contains all number terms, identifier terms, and other
atomic terms, as well as all list, vector, hash, box, and prefab struct terms
containing @shape{Datum} elements. That is, @shape{Datum} contains any term the
corresponds to a @racket[read]able value.

The @shape{Datum} shape represents the intention to use the term as a literal
within a @racket[quote] expression, or to convert it to a compile-time value
using @racket[syntax->datum].

There is no syntax class corresponding to @shape{Datum}.

Let's design the macro @racket[my-case1], which is like @racket[my-evcase1] from
@exercise-ref["compound:my-evcase1"] and @secref["defshape-same-diff"] except
that each clause's comparison value is given as a datum rather than an
expression. That is, the macro's shape is:
@codeblock{
;; (my-case1 Expr [Datum Expr] ...) : Expr
}
Here is an example:
@racketblock[
(my-case1 (begin (printf "got a coin!\n") (* 5 5))
  [5 "nickel"] [10 "dime"] [25 "quarter"])
(code:comment "expect print once, \"quarter\"")
]
Here is an implementation:
@examples[#:eval the-eval #:no-result
(code:comment "(my-case1 Expr [Datum Expr] ...) : Expr")
(define-syntax my-case1
  (syntax-parser
    [(_ to-match:expr [datum_ result:expr] ...)
     #'(let ([tmp to-match])
         (cond [(equal? tmp (Quote datum_)) result] ...))]))
]
I often spell out @racket[quote] in a syntax template when it is applied to a
term containing pattern variables, to remind myself that the @racket[quote]d
``constant'' can vary based on the macro's arguments.

Here is another implementation:
@examples[#:eval the-eval #:no-result
(code:comment "(my-case1 Expr [Datum Expr] ...) : Expr")
(define-syntax my-case1
  (syntax-parser
    [(_ to-match:expr [datum_ result:expr] ...)
     #'(let ([tmp to-match])
         (cond [(equal? tmp datum_) result] ...))]))
]
This implementation is @emph{wrong}, because the @shape{Datum} arguments are not
used within a @racket[quote] expression. Never implicitly treat a @shape{Datum}
as an @shape{Expr}! One obvious problem is that not every datum is
self-quoting. The following example should return @racket["matched"], but it
raises an error instead:
@examples[#:eval the-eval #:label #f
(eval:error
(my-case1 (list 123)
  [(123) "matched"]))
]
Even a datum that is normally self-quoting can carry a lexical context with an
alternative @racket[#%datum] binding that gives it some other behavior. For
example:
@examples[#:eval the-eval #:label #f
(eval:error
(let-syntax ([#%datum (lambda (stx) (raise-syntax-error #f "no self-quoting!" stx))])
  (my-case1 '2 [1 'one] [2 'two] [3 'lots])))
]
This particular example is admittedly uncommon. A more common problem is that
the datum is computed by a macro, and depending on how it is coerced to a syntax
object it may or may not get a lexical context with Racket's @racket[#%datum]
binding. Avoid all of these problems by treating @shape{Datum} and @shape{Expr}
as distinct shapes. If you have a datum and you want an expression that
evaluates to the same value at run time, put the datum in a @racket[quote]
expression.

@exercise[#:tag "little:my-case"]{Generalize @racket[my-case1] to
@racket[my-case], which has a list of datums in each clause. That is, the macro
has the following shape:
@codeblock{
;; (my-case Expr [(Datum ...) Expr] ...) : Expr
}}


@; ------------------------------------------------------------
@section[#:tag "quasiquote"]{Quasiquotation: A Little Datum Language}

@;{
What makes this a ``language''?
- Complexity of the shape?
- Degree of interpretation by macro?
}

@codeblock{
;; (my-quasiquote QD) : Expr
}

@shape{QD} (``quasidatum'')

@codeblock{
;; QD   ::= (escape Expr)
;;        | (QD ...)
;;        | Atom
;; Atom ::= Identifier | Number | Boolean | String
}


@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class atom
    #:attributes ()
    (pattern d
             #:when (let ([v (syntax->datum #'d)])
                      (or (symbol? v) (number? v) (boolean? v) (string? v)))))
  (define-syntax-class quasidatum
    #:attributes (code) (code:comment "Expr")
    #:literals (escape)
    (pattern (escape code:expr))
    (pattern (elem:quasidatum ...)
             #:with code #'(list elem.code ...))
    (pattern a:atom
             #:with code #'(Quote a))))
]

@examples[#:eval the-eval #:no-result
(define-syntax my-quasiquote
  (syntax-parser
    [(_ qd:quasidatum)
     #'qd.code]))
]


@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class quasidatum
    #:attributes (const?    (code:comment "Boolean")
                  code)     (code:comment "Expr")
    #:literals (escape)
    (pattern (escape code:expr)
             #:attr const? #f)
    (pattern (elem:quasidatum ...)
             #:attr const? (andmap values (datum (elem.const? ...)))
             #:with code (if (datum const?)
                             #'(Quote (elem ...))
                             #'(list elem.code ...)))
    (pattern a:atom
             #:attr const? #t
             #:with code #'(Quote a))))
]


@(close-eval the-eval)
