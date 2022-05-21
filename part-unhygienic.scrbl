;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string racket/struct-info syntax/transformer racket/syntax
                     racket/contract rackunit))

@(define the-eval (make-malr-eval))
@(the-eval '(require racket/match))

@; ============================================================
@title[#:tag "unhygienic"]{Unhygienic Macros}

Recall the definition of a @tech{hygienic} macro: definition-site binders do not
capture use-site references, and use-site binders do not capture definition-site
references. Hygienic macros can still implement binding forms (recall
@racket[my-and-let], for example, from @secref["basic-id"]), but the bound names
must be given as arguments.

Sometimes, though, it is useful for a macro to bind names that are visible to
the macro use site without receiving the names as explicit arguments. Such
macros are @deftech{unhygienic}; we also say that they ``break
hygiene''. Unhygienic macros are mainly divided into two groups; I'm going to
call them @tech{clean unhygienic macros} and @tech{unclean hygienic macros}, and
you can't stop me.


@; ------------------------------------------------------------
@section[#:tag "clean-unhygienic"]{Clean Unhygienic Macros}

A @deftech{clean unhygienic macro} defines names that are not given as
@shape{Id} arguments, but are based on one or more @shape{Id} arguments.

The canonical example of a clean unhygienic macro is @racket[struct]: it defines
the predicate and accessor functions (as well as a few other names) based on the
identifier given as the struct name and the identifers naming the fields. A
greatly simplified version of @racket[struct] could be given the following
shape:

@codeblock{
;; (struct s:Id (f:Id ...)) : Body[{s,s?,s-f...}]
}

As an example, let's design a macro @racket[my-hash-view], which puts a
@racket[struct]-like interface on symbol-keyed hashes. It has the following
shape:

@codeblock{
;; (my-hash-view v:Id (f:Id ...)) : Body[{v,v?,v-f...}]
}

It should have the following behavior:

@racketblock[
(my-hash-view point (x y))
(code:comment "defines point, point?, point-x, point-y")
(point 1 2)
(code:comment "expect (hash 'x 1 'y 2)")
(point? (hash 'x 3 'y 4))
(code:comment "expect #t")
(point? (hash 'x 3 'y 4 'z 5))
(code:comment "expect #t")
(point? (hash 'x 6))
(code:comment "expect #f")
(point-x (hash 'x 7 'y 8))
(code:comment "expect 7")
]

Let's consider what code we could use to implement the intended behavior.

@examples[#:eval the-eval #:no-result #:label #f
(begin
  (define (point x y)
    (hash 'x x 'y y))
  (define (point? v)
    (and (hash? v) (hash-has-key? v 'x) (hash-has-key? v 'y)))
  (define (point-x v)
    (unless (point? v)
      (raise-argument-error 'point-x "point?" v))
    (hash-ref v 'x))
  (define (point-y v)
    (unless (point? v)
      (raise-argument-error 'point-y "point?" v))
    (hash-ref v 'y)))
]

We need to produce the identifiers @racket[point?], @racket[point-x], and
@racket[point-y]. This code also has the string literal @racket["point?"]; we
could compute it at run time (as we did in @secref["first"]), but in this
example let's go ahead and compute it at compile time. The other part of the
code that is a bit tricky to produce is the body of the constructor function:
@racket[(hash 'x x 'y y)]. The @racket[hash] arguments do not consist of a
single repeated term, but rather each repetition consists of two
terms. Fortunately, Racket's syntax template support ungrouped repetition using
the @racket[~@] template form.

Before we continue to the implementation of the macro, we can also use this
hand-expansion to run our tests, to check that the expansion works before we
automate its generation with the macro.

@examples[#:eval the-eval #:label #f
(check-equal? (point 1 2) (hash 'x 1 'y 2))
(check-pred point? (hash 'x 3 'y 4))
(check-pred point? (hash 'x 3 'y 4 'z 5))
(check-equal? (point? (hash 'x 6)) #f)
(check-equal? (point-x (hash 'x 7 'y 8)) 7)
(check-exn #rx"point-x: contract violation"
           (lambda () (point-x (hash 'z 9))))
]

The tests pass, so let's move on the the macro.

Given the identifier representing the use-site name @racket[point], how do we
compute an identifier @racket[point?] that acts like it also came from the macro
use site? Using ordinary Racket functions we can compute the symbol
@racket['point?] given the symbol @racket['point]. The extra step the macro must
perform is to transfer the @emph{lexical context} from the original
@racket[point] identifier to the new identifier. The primitive mechanism for
doing that is @racket[datum->syntax]: its first argument is an existing syntax
object to take the lexical context from, and the second argument is a datum to
wrap as the new syntax object. So the following is the process for computing the
@racket[point?] identifier from the @racket[point] identifier:

@racketblock[
(define point-id #'point)
(define point-symbol (syntax->datum point-id))
(define point?-symbol (string->symbol (format "~a?" point-symbol)))
(define point?-id (datum->syntax point-id point?-symbol))
]

The @racket[format-id] automates this process. It takes the lexical context
source object first, then a restricted format string (allowing only @litchar{~a}
placeholders), and then the format strings arguments. Unlike @racket[format],
@racket[format-id] automatically unwraps identifiers in the format string
arguments to their symbol contents.

@racketblock[
(define point?-id (format-id point-id "~a?" point-id))
]

Additionally, @racket[format-id] with the @racket[#:subs? #t] option builds the
identifier with a syntax property (a way of attaching extra information to a
syntax object) indicating the positions of the original identifier
components. This information lets, for example, DrRacket draw binding arrows to
parts of identifiers.

@racketblock[
(define point?-id (format-id point-id "~a?" point-id #:subs? #t))
]

Finally, instead of using @racket[quasisyntax] and @racket[unsyntax]
(@litchar{#`} and @litchar{#,}) to insert the results of compile-time
computation into syntax templates, we can use @racket[#:with] or
@racket[with-syntax] to bind secondary syntax pattern variables to the computed
terms.

Here is the macro definition:

@examples[#:eval the-eval #:no-result #:label #f
(define-syntax my-hash-view
  (syntax-parser
    [(_ name:id (field:id ...))
     #:with name? (format-id #'name "~a?" #'name #:subs? #t)
     #:with name?-string (format "~a?" (syntax->datum #'name)) (code:comment "implicit datum->syntax")
     #:with (name-field ...) (for/list ([fieldname (in-list (datum (field ...)))])
                               (format-id #'name "~a-~a" #'name fieldname #:subs? #t))
     (code:comment "name? : Id, name?-string : Datum, (name-field ...) : (Id ...)")
     #'(begin
         (define (name field ...)
           (hash (~@ (Quote field) field) ...))
         (define (name? v)
           (and (hash? v) (hash-has-key? v (Quote field)) ...))
         (define (name-field v)
           (unless (name? v)
             (raise-argument-error (Quote name-field) (Quote name?-string) v))
           (hash-ref v (Quote field)))
         ...)]))
]
@examples[#:eval the-eval #:hidden
(my-hash-view point (x y))
(assert (equal? (point 1 2) (hash 'x 1 'y 2)))
(assert (point? (hash 'x 3 'y 4)))
(assert (point? (hash 'x 3 'y 4 'z 5)))
(assert (not (point? (hash 'x 6))))
(assert (equal? (point-x (hash 'x 7 'y 8)) 7))
]

Let's run the tests against the macro implementation:

@examples[#:eval the-eval #:label #f
(code:comment "(my-hash-view point (x y)))")
(check-equal? (point 1 2) (hash 'x 1 'y 2))
(check-pred point? (hash 'x 3 'y 4))
(check-pred point? (hash 'x 3 'y 4 'z 5))
(check-equal? (point? (hash 'x 6)) #f)
(check-equal? (point-x (hash 'x 7 'y 8)) 7)
(check-exn #rx"point-x: contract violation"
           (lambda () (point-x (hash 'z 9))))
]


@exercise[#:tag "unh-implicit"]{The @racket[#:with name?-string] binding in the
definition above implicitly converts the string result of @racket[format] into a
syntax object. That's okay, as long as we treat @racket[name?-string] as a
@shape{Datum}. What happens if we treat it like an @shape{Expr} instead? Find
out by replacing @racket[(Quote name?-string)] with @racket[name?-string] in the
macro's syntax template.}


@; ------------------------------------------------------------
@section[#:tag "unclean-hygienic"]{Unclean Unhygienic Macros}

An @deftech{unclean unhygienic macro} defines names that are not based on any
@shape{Id} arguments.

The canonical example of an unclean unhygienic macro is a @racket[while] loop
that binds the name @racket[break] to an escape continuation to exit the loop.




@(close-eval the-eval)
