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
call them @tech{clean unhygienic macros} and @tech{unclean unhygienic macros},
and you can't stop me.


@; ------------------------------------------------------------
@section[#:tag "clean-unhygienic"]{Clean Unhygienic Macros}

A @deftech{clean unhygienic macro} defines names that are not given as
@shape{Id} arguments, but are based on one or more @shape{Id} arguments.

A good example of a clean unhygienic macro is @racket[struct]: it defines the
predicate and accessor functions (as well as a few other names) based on the
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
terms. Fortunately, Racket's syntax templates support multi-term repetition
using the @racket[~@] template form.

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

@exercise[#:tag "unh-enum"]{Update the implementation of @racket[my-hash-view]
to allow field names to have different hash keys. That is, generalize the shape
to the following:
@codeblock{
;; (my-hash-view v:Id [fs:FieldSpec ...]) : Body[{v,v?,v-fs.fn...}]
;; where FieldSpec ::= fn:Id | [fn:Id #:key Datum]
}
Here is an example to illustrate the intended behavior:
@racketblock[
(my-hash-view post (author [link #:key resource_href]))
(define post1 (hash 'author "Ryan" 'resource_href "/malr/unhygienic.html"))
(post-link post1) (code:comment "expect \"/malr/unhygienic.html\"")
]
Hint: use the @tech{common meaning} interface strategy.}

@exercise[#:tag "unh-static"]{Update the implementation of @racket[my-hash-view]
so that the hash view name acts both as a constructor and as a @racket[match]
pattern name. That is, the hash view name should be statically bound to a
compile-time struct implementing both the procedure interface and the
@racket[match] expander interface. You should define the actual constructor
function with a different name and expand to it using
@racket[make-variable-like-transformer]. For the match expander, use the
@racket[?] and @racket[app] match pattern forms. That is, as a match pattern,
@racket[point] behaves as follows:
@racketblock[
(point _x-pat _y-pat)
==>
(? point? (app point-x _x-pat) (app point-y _y-pat))
]}

@exercise[#:tag "unh-static2" #:stars 1]{Update your solution to
@exercise-ref["unh-static"] to also support hash view extension (or
``subtyping''). That is, the value statically bound to hash-view name must
support three interfaces: the procedure interface, the @racket[match] expander
interface, and a private interface that carries enough information to support
view extension.

Here are some examples to illustrate the expected behavior:
@racketblock[
(my-hash-view point (x y))
(my-hash-view point3 #:super point (z))
(define p3 (point3 1 2 3))
(point? p3) (code:comment "expect #t")
(point3? p3) (code:comment "expect #t")
(point-x p3) (code:comment "expect 1")
(point3-z p3) (code:comment "expect 3")
(match p3 [(point x y) (+ x y)]) (code:comment "expect 3")
(match p3 [(point3 x y z) (+ x y z)]) (code:comment "expect 6")
]}

@; ------------------------------------------------------------
@section[#:tag "unclean-hygienic"]{Unclean Unhygienic Macros}

An @deftech{unclean unhygienic macro} defines names that are not based on any
@shape{Id} arguments.

The canonical example of an unclean unhygienic macro is a @racket[while] loop
that binds the name @racket[break] to an escape continuation to exit the loop.

What lexical context should the macro use to create the @racket[break] binder?
The best candidate here is the lexical context of the whole macro use. In a
@racket[syntax-parser] form, this is available through the name
@racket[this-syntax]. (You might wonder whether @racket[this-syntax] is bound
unhygienically. It isn't. In fact, we'll talk about the mechanism it uses in
@secref["stxparam"].)

Here is the macro definition:

@examples[#:eval the-eval #:no-result
(code:comment "(while Expr Expr{break} ...+) : Expr")
(define-syntax while
  (syntax-parser
    [(_ condition:expr loop-body:expr ...+)
     #:with break (datum->syntax this-syntax 'break)
     #'(let/ec break
         (let loop ()
           (when condition
             loop-body ...
             (loop))))]))
]

With this macro, we can finally write FORTRAN in Racket:

@examples[#:eval the-eval #:label #f
(define ns '(2 3 4 5 6))
(define sum 0)
(while (pair? ns)
  (when (integer? (sqrt (car ns))) (break))
  (set! sum (+ sum (car ns)))
  (set! ns (cdr ns)))
sum
]

Now let's write the macro @racket[forever] that uses @racket[while] as the
helper macro. That is:

@racketblock[
(forever _loop-body)
==>
(while #t _loop-body)
]

It should be trivial, right? Here's a definition:

@examples[#:eval the-eval #:no-result
(code:comment "(forever Expr{break} ...+) : Expr")
(define-syntax forever
  (syntax-parser
    [(_ loop-body:expr ...+)
     #'(while #t loop-body ...)]))
]

But if we try to use @racket[break] in the loop body, this happens:

@examples[#:eval the-eval #:label #f
(define counter 0)
(eval:error
(forever
  (set! counter (add1 counter))
  (unless (< counter 5) (break))
  (printf "counter = ~s\n" counter)))
]

In a module, this wouldn't even compile, because @racket[break] is unbound.

What went wrong? Here is one explanation: The @racket[forever] example expands
into a use of @racket[while], which expands into code that binds @racket[break]
with the lexical context of the @racket[while] expression. But the lexical
context of the @racket[while] expression is from the definition site of
@racket[forever], not the use site of @racket[forever] in the example! Given
that those are not necessarily the same, there's no reason to expect the example
to work.

On the other hand, it's not clear what makes the two sites different,
either. What is a ``site'', anyway? The definition of @racket[forever] and the
example use of @racket[forever] are both top level interactions (of this
Scribble document's evaluator, specifically); what makes them distinct?

We need to refine our definition of hygiene slightly. Each time a macro is
invoked, it is considered to have a different ``site''. More precisely, the
meaning of @emph{references} in the macro's syntax template is determined by its
definition site, but an extra marker is added that distinguishes binders
introduced by different macro invocations. In the terminology of Racket's
hygiene model, this extra marker is called a @tech/reference{macro-introduction
scope}.

We can ``fix'' the implementation of @racket[forever] by adjusting the lexical
context on the syntax object representing the use of the @racket[while] macro
(but not on any of its subterms) to be the same as the use of the
@racket[forever] macro. We do that by using @racket[syntax-e] to unwrap just the
outer layer of syntax, and then we use @racket[datum->syntax] to rebuild it with
the lexical context of @racket[this-syntax]. Here is the implementation:

@examples[#:eval the-eval #:no-result
(code:comment "(forever Expr{break} ...+) : Expr")
(define-syntax forever
  (syntax-parser
    [(_ loop-body:expr ...+)
     (define code
       #'(while #t loop-body ...))
     (datum->syntax this-syntax (syntax-e code))]))
]

Now the example works:

@examples[#:eval the-eval #:label #f
(define counter 0)
(forever
  (set! counter (add1 counter))
  (unless (< counter 5) (break))
  (printf "counter = ~s\n" counter))
]

With this approach, @racket[break] is visible to the loop body (well, assuming
that the loop body terms have the same lexical context as the term representing
the whole call to @racket[forever], which is not necessarily true), but it is
not visible to the code introduced by the @racket[forever] macro.

Here's another approach that works if we want to use @racket[break] in the macro
as well as making it visible to the loop body:

@examples[#:eval the-eval #:no-result
(code:comment "(do-while Expr Body{break} ...+) : Expr")
(define-syntax do-while
  (syntax-parser
    [(_ condition:expr loop-body:expr ...+)
     #:with break/user (datum->syntax this-syntax 'break)
     #'(while #t
         (let ([break/user break])
           loop-body ...)
         (unless condition (break)))]))
]

@lesson{Unhygienic macros are difficult to use as helper macros --- that is, as
the targets of expansion.}


@; ------------------------------------------------------------
@section[#:tag "mixed-hygienic"]{Optionally-Hygienic Macros}

Consider Racket's @racket[require] form. For example,

@racketblock[
(require racket/list)
]

locally binds the names @racket[first], @racket[second], and so on, even though
those names are not given as binder @shape{Id} arguments to @racket[require]. In
fact, @racket[require] is acting as an unclean unhygienic binding form here ---
its argument, @racket[racket/list] is an identifier, but @racket[require]'s
argument shape is @shape{RequireSpec}, which has a @shape{ModulePath} variant,
which has an identifier variant. We could also consider @racket[(require (lib
"racket/list.rkt"))], which means the same thing.

On the other hand, in the following,

@racketblock[
(require (only-in racket/list first [last final]))
]

the @racket[first] identifier is used for the binding of the @racket[first]
import, and the @racket[final] identifier is used for the binding of the import
that @racketmodname[racket/list] exports as @racket[last]. So this particular
usage of @racket[require] is hygienic!

One way to mitigate the difficulty that unhygienic macros cause is to give them
hygienic options. For example, we could extend @racket[while] with an optional
clause for specifying the name to bind to the escape continuation. If the
clause is present, the macro binds the given name, and it is hygienic; if the
clause is absent, it generates the name unhygienicially. Here is the optional
clause shape:

@codeblock{
;; MaybeBreakClause ::= ε | #:break Id
}

Instead of defining a (splicing) syntax class for it, though, let's just handle
it inline within the macro's syntax pattern using the @racket[~optional] pattern
form. If an @racket[~optional] pattern is absent, then all of its pattern
variables are bound to the value @racket[#f] (note: not the syntax object
representing the term @racket[#f]). Normally, only syntax-valued attributes can
be used within syntax templates, but the template form @racket[~?] can
dynamically ``catch'' false-valued attributes in its first sub-template and fall
back to its second sub-template. We can define the macro as follows:

@examples[#:eval the-eval #:no-result
(code:comment "(while Expr MaybeBreakClause Expr{break} ...+) : Expr")
(define-syntax while
  (syntax-parser
    [(_ condition:expr (~optional (~seq #:break break-name:id))
        loop-body:expr ...+)
     #:with default-break (datum->syntax this-syntax 'break)
     #'(let/ec (~? break-name default-break)
         (let loop ()
           (when condition
             loop-body ...
             (loop))))]))
]

Here is an example:

@;{
@examples[#:eval the-eval #:label #f
(define n 2022)
(while #t #:break stop
  (cond [(= n 1) (printf "\n") (stop)]
        [(even? n) (printf "⌄") (set! n (quotient n 2))]
        [(odd? n) (printf "⌃") (set! n (add1 (* n 3)))]))
]
}

@examples[#:eval the-eval #:label #f
(define n 2022)
(while #t #:break stop
  (cond [(= n 1) (printf "\n") (stop)]
        [(even? n) (printf "↑") (set! n (quotient n 2))]
        [(odd? n) (printf "↓") (set! n (add1 (* n 3)))]))
]

Here is the equivalent definition with a separate syntax class:

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-splicing-syntax-class maybe-break-clause
    #:attributes (break-name) (code:comment "(U #f Syntax[Id])")
    (pattern (~seq #:break break-name:id))
    (pattern (~seq) #:attr break-name #f)))

(code:comment "(while Expr MaybeBreakClause Expr{break} ...+) : Expr")
(define-syntax while
  (syntax-parser
    [(_ condition:expr bc:maybe-break-clause
        loop-body:expr ...+)
     #:with default-break (datum->syntax this-syntax 'break)
     #'(let/ec (~? bc.break-name default-break)
         (let loop ()
           (when condition
             loop-body ...
             (loop))))]))
]

@; ------------------------------------------------------------
@section[#:tag "stxparam"]{Syntax Parameters}

Another alternative to @tech{unclean unhygienic macros} is to define a single
name that takes on different meanings in different contexts. This is analogous
to run-time @tech/reference{parameter} values, so the feature is called a
@emph{syntax parameter}.

FIXME: finish


@(close-eval the-eval)
