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
@title[#:tag "rec-shapes"]{Recursive Shapes}


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
@section[#:tag "rec-datum"]{Datum as a Recursive Shape}

Could we write a definition of @shape{Datum} rather than treating it as a basic
(that is, primitive) shape? The full definition would be quite complicated,
since Racket has many kinds of @racket[read]able values, and it occasionally
adds new ones. Let's simplify the question to datum terms built from
identifiers, numbers, booleans, strings, and proper lists; let's call this shape
@shape{SimpleDatum}. We can define it as a @emph{recursive shape} as follows:
@codeblock{
;; SimpleDatum ::= (SimpleDatum ...) | SimpleAtom
;; SimpleAtom  ::= Identifier | Number | Boolean | String
}
I have collected the base cases into a separate shape, @shape{SimpleAtom}, for
convenience.

Like the corresponding shapes, the @racket[simple-datum] syntax class is
recursive; the @racket[simple-atom] syntax class is not. Let's discuss
@racket[simple-atom] first.

The @racket[simple-atom] syntax class presents a challenge: There is a built-in
syntax class for identifiers (@racket[id]), but how do we check whether a term
contains a number, a boolean, or a string? Given a syntax object, we can extract
the corresponding plain Racket value by calling @racket[syntax->datum]. Then we
can use the ordinary @racket[number?], @racket[boolean?], and @racket[string?]
predicates. An identifier is just a syntax object containing a symbol, so we can
cover the identifier case by adding a @racket[symbol?] predicate check to the
others. Finally, we perform this check using a @racket[~fail] pattern; if the
check fails, then the syntax class does not accept the term. Here is the
definition:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "simple-atom? : Any -> Boolean")
  (define (simple-atom? v)
    (or (symbol? v) (number? v) (boolean? v) (string? v)))
  (define-syntax-class simple-atom
    #:attributes () ;; #:opaque (code:comment "!!")
    (code:comment "(pattern a #:when (simple-atom? (syntax->datum #'a)))")
    (pattern a #:and (~fail #:unless (simple-atom? (syntax->datum #'a))))))
]
In most cases, it is better to use @racket[#:when] to perform checks like this,
but this is one of the few cases where we don't want a ``post-traversal'' check
that dominates other matching failures. The difference between the two only
affects the way errors are reported.

The @racket[simple-datum] syntax class is straightforward. The recursive case in
the shape simply translates to a syntax pattern with recursive syntax class
annotations:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class simple-datum
    #:attributes ()
    (pattern (elem:simple-datum ...))
    (pattern atom:simple-atom)))
]

There are no attributes, because the only interpretation that
@shape{SimpleDatum} supports can be achieved with @racket[quote] or
@racket[syntax->datum] on the term itself.

@;{
Actually, the following definition of @racket[simple-atom] is slightly better
for error reporting. The previous version uses @racket[#:when], which performs a
post-traversal check, but this is one of the few cases where an early check is
more appropriate.
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class simple-atom
    #:attributes ()
    (pattern (~and a (~fail #:unless (simple-atom? (syntax->datum #'a)))))))
]
@; FIXME: Or just use #:opaque, maybe? Double-check that it works.
}


@; ------------------------------------------------------------
@section[#:tag "quasiquote"]{Quasiquotation}

Let's define @racket[my-quasiquote], a simple version of Racket's
@racket[quasiquote] macro. Its argument has a shape like @shape{SimpleDatum},
except that it can have ``escapes'' to Racket expressions so we can compute
values to insert into the result. The shape of the macro is the following:
@codeblock{
;; (my-quasiquote QuasiDatum) : Expr
}
where @shape{QuasiDatum} is defined as follows:
@codeblock{
;; QuasiDatum ::= (escape Expr)
;;              | (QuasiDatum ...)
;;              | SimpleAtom
}
What does @racket[escape] mean in this shape definition? That is, how do we
recognize the @racket[escape] form of @shape{QuasiDatum}. There are two
possibilities: we could recognize @racket[escape] either as a @emph{symbolic
literal} or as a @emph{reference literal}. For this example, we'll recognize
@racket[escape] by reference; we'll show an example of symbolic literals later
in FIXME-REF.

Recognizing @racket[escape] as a reference means that there must be a binding of
@racket[escape] for the reference to refer to. Since we don't intend
@racket[escape] to have any meaning as a Racket expression, we should define it
as a macro that always raises a syntax error:
@examples[#:eval the-eval #:no-result
(define-syntax escape
  (lambda (stx) (raise-syntax-error #f "illegal use of escape" stx)))
]
@; Note: syntax-parse literal checking is confused by top-level bindings, so
@; actually use a module import instead.
@examples[#:eval the-eval #:hidden
(module escape racket/base
  (require (for-syntax racket/base))
  (provide escape)
  (define-syntax escape
    (lambda (stx) (raise-syntax-error #f "illegal use of escape" stx))))
(require 'escape)
]
The error only occurs if the macro is expanded by the Racket macro expander; our
macros and syntax classes can still recognize references to it without
triggering the error.

Now we can implement the @racket[quasi-datum] syntax class. We declare
@racket[escape] as a (reference) literal using @racket[#:literals]; then
occurrences of @racket[escape] in the syntax patterns are treated as literals
instead of as pattern variables. Here is the definition, without attributes:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class quasi-datum
    #:literals (escape)
    (pattern (escape code:expr))
    (pattern (elem:quasi-datum ...))
    (pattern a:simple-atom)))
]

What interface should we give to the @racket[quasi-datum] syntax class? Recall
the interface design options from @secref["enum-shapes-def"]. Most of them are
applicable here; with the possible exception of the ``common meaning''
approach. Let's use the ``macro behavior'' approach. The @racket[my-quasiquote]
interprets @shape{QuasiDatum} as instructions to construct a value from a
mixture of constants and values computed by escaped Racket expressions. We can
represent that with a syntax attribute, @racket[code], containing an expression
that produces the @shape{QuasiDatum}'s value. Here is the definition:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class quasi-datum
    #:attributes (code) (code:comment "Expr")
    #:literals (escape)
    (pattern (escape code:expr))
    (pattern (elem:quasi-datum ...)
             #:with code #'(list elem.code ...))
    (pattern a:simple-atom
             #:with code #'(Quote a))))
]
That is, the @racket[escape] form contains the necessary expression directly;
the list form constructs a list from the values constructed by its components;
and an atom is interpreted as a value by quoting it.

The macro simply expands into its argument's @racket[code] expression:
@examples[#:eval the-eval #:no-result
(define-syntax my-quasiquote
  (syntax-parser
    [(_ qd:quasi-datum)
     #'qd.code]))
]

Here are some examples:
@examples[#:eval the-eval #:label #f
(my-quasiquote (1 2 () abc xyz))
(my-quasiquote (1 2 (escape (+ 1 2))))
(my-quasiquote ((expression (+ 1 2)) (value (escape (+ 1 2)))))
(my-quasiquote (a (b (c (d (e (f (escape (string->symbol "g")))))))))
]

The behavior of this example is questionable, though:
@examples[#:eval the-eval #:label #f
(my-quasiquote (1 2 (escape 3 4)))
]

Do we want @racket[(escape 3 4)] to be interpreted as a three-element list, or
do we want to consider it an ill-formed @racket[escape] form and report an
error? I prefer to consider it an error. That is, whenever the @racket[quasi-datum]
parser gets a term that starts @racket[(escape HOLE ...)], it should
@emph{commit} to parsing it according to the @racket[escape] pattern. This
problem is similar to the problem with @racket[=>] clauses in
@secref["enum-overlap"]. The solution is the same too: We must put the
@racket[escape] pattern before any other pattern it might overlap with, and we
must add a cut (@racket[~!]) immediately after the @racket[escape] literal. Here
is the updated code:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class quasi-datum
    #:attributes (code) (code:comment "Expr")
    #:literals (escape)
    (pattern (escape ~! code:expr))
    (pattern (elem:quasi-datum ...)
             #:with code #'(list elem.code ...))
    (pattern a:simple-atom
             #:with code #'(Quote a))))
]
@examples[#:eval the-eval #:hidden
;; --- Get the updated syntax class def ---
(define-syntax my-quasiquote
  (syntax-parser
    [(_ qd:quasi-datum)
     #'qd.code]))
]

Now the example signals an error instead:
@examples[#:eval the-eval #:label #f
(eval:error (my-quasiquote (1 2 (escape 3 4))))
]

There's one remaining issue with this implementation. Consider the following
example:
@examples[#:eval the-eval #:label #F
(my-quasiquote (1 2 3 4 5))
]
This example has no escapes, so its result could be implemented with a simple
@racket[quote] expression. But this is how the macro expands:
@racketblock[
(my-quasiquote (1 2 3 4 5))
==>
(list (Quote 1) (Quote 2) (Quote 3) (Quote 4) (Quote 5))
]

Let's optimize @racket[my-quasiquote] so that it uses @racket[quote] at the
highest levels possible. Here's one strategy: we add an boolean-valued
@racket[const?] attribute that is true when a term has no escapes. A list
@shape{QuasiDatum} is constant if all of its elements are constant; in that
case, its code can be simply the quotation of the elements. Here is the updated
syntax class:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class quasi-datum
    #:attributes (const?    (code:comment "Boolean")
                  code)     (code:comment "Expr")
    #:literals (escape)
    (pattern (escape code:expr)
             #:attr const? #f)
    (pattern (elem:quasi-datum ...)
             #:attr const? (andmap values (datum (elem.const? ...)))
             #:with code (if (datum const?)
                             #'(Quote (elem ...))
                             #'(list elem.code ...)))
    (pattern a:simple-atom
             #:attr const? #t
             #:with code #'(Quote a))))
]
@examples[#:eval the-eval #:hidden
;; --- Get the updated syntax class def ---
(define-syntax my-quasiquote
  (syntax-parser
    [(_ qd:quasi-datum)
     #'qd.code]))
(assert (equal? (my-quasiquote (1 2 (escape (+ 1 2)))) '(1 2 3)))
(assert (equal? (my-quasiquote (1 2 3)) '(1 2 3)))
]

@; FIXME: How can we test for optimization?


@exercise[#:tag "rec:qq-ignore"]{Extend the definition of @shape{QuasiDatum} as
follows:
@codeblock{
;; QuasiDatum ::= ... | (ignore-me QuasiDatum)
}
An @racket[ignore-me] wrapper is simply omitted from the constructed value; that
is, the @shape{QuasiDatum} @racket[(ignore-me _qd)] is equivalent to
@racket[_qd]. For example:
@racketblock[
(my-quasiquote (1 2 (ignore-me 3) (escape (* 2 2))))
(code:comment "expect '(1 2 3 4)")
(my-quasiquote (1 (ignore-me 2) (ignore-me (ignore-me 3))))
(code:comment "expect '(1 2 3)")
]

Start with the unoptimized version of the @racket[quasi-datum] syntax
class. After you have updated (and tested) that version, implement a similar
optimization for the updated @shape{QuasiDatum} shape. For example, the second
example above should expand directly to @racket[(Quote (1 2 3))]. (Hint: What
assumption made by the original optimization does the updated shape violate?)}


@(close-eval the-eval)
