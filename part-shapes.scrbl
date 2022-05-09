;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse racket/match rackunit syntax/macro-testing))

@(define the-eval (make-base-eval))
@(the-eval '(require (only-in racket/base [quote Quote] [syntax Syntax])
                     (for-syntax racket/base racket/syntax
                                 (only-in racket/base [quote Quote] [syntax Syntax])
                                 syntax/parse syntax/macro-testing)))

@; ============================================================
@title[#:tag "shapes" #:version ""]{Terms and Shapes}

This section introduces terminology for talking about the pieces of Racket
programs and their interpretation.


@; ------------------------------------------------------------
@section[#:tag "shapes-terms"]{Terms}

Here is some Racket code:

@racketblock[
    (define (map f xs)
      (cond [(pair? xs)
             (cons (f (car xs)) (map f (cdr xs)))]
            [(null? xs) '()]))
]

The code is a tree of terms. A @deftech{term} is, roughly, an atom or a
parenthesized group of terms. So all of the following are terms:
@itemlist[

@item{@racket[define], @racket[map], @racket[xs], @racket[pair?] --- More
specifically, these are @emph{identifier terms}.}

@item{@racket[(pair? xs)], @racket[(f (car xs))], @racket[[(null? xs) '()]] ---
More specifically, these are @emph{list terms}.}

@item{@racket['()] --- This is also a list term, because it is read as
@racket[(Quote ())].}

]

The following is not a term:
@itemlist[

@item{@racket[map f] --- That's two terms.}

]

The following are terms and also occur in the program above, even though it
might not be immediately apparent:
@itemlist[

@item{@racket[(f xs)] --- That's because @racket[(map f xs)] is the same as
@racket[(map ! (f xs))], which is also the same as @racket[(map ! (f ! (xs !
())))].}

@item{@racket[quote] --- Because it's a subterm of @racket['()], which is the
same as @racket[(Quote ())].}

]

Here are some other terms that don't appear in the program above:
@itemlist[

@item{@racket[#t], @racket[5], @racketvalfont{#e1e3},
@racket["racket-lang.org"], @racket[#(1 2 3)], @racket[#s(point 3 4)],
@racket[#:unless], @racket[#rx"[01]+"] --- A @emph{boolean term}, two
@emph{number terms}, a @emph{string term}, and so on.}

]

Racket represents terms using @emph{syntax objects}, a kind of value.

It will be helpful to keep the two levels separate (term vs value
representation), but that's hard, because we don't have enough distinct terms
(err, I mean words) to name everything. In some cases, the context should either
make the usage clear or make the distinction moot. In some cases, I'll
disambiguate by saying, for example, @emph{identifier term} vs @emph{identifier
value}.


@; ------------------------------------------------------------
@section[#:tag "shapes-interps"]{Interpretations of Terms}

What is an expression?

The concept of ``expression'' doesn't simply refer to some subset of
terms. @emph{Any} term can be an expression, given the right context. And a term
might be an expression when used in one place but not when used in another. Is
the identifier @racket[f] an expression? In the example code above, the first
occurrence of @racket[f] is not an expression, but the second and third
occurrences are expressions. It depends on context --- that is, where the term
appears in the code. The term @racket[f] isn't an expression when it occurs in
the function definition's formal parameter list, but it is an expression when it
occurs in operator position of an application. What is an ``application''? Well,
it's a kind of expression --- and so we have to keep looking outward to figure
out what's going on.

Here's the reasoning for the second and third occurrences of @racket[f] being
expressions: The example is a use of @racket[define], and the rule for
@racket[define] is that the body is an expression (that's an oversimplification,
actually). The body is a @racket[cond] expression, and a @racket[cond]
expression's arguments are ``clauses'', which are not expressions themselves,
but consist of two expressions grouped together (again, oversimplified). The
second expression of the first @racket[cond] clause is a function call to
@racket[cons], so its first argument is an expression. And that expression is a
function call (because @racket[f] is bound as a variable), so that @racket[f] is
an expression. And that's how we know, starting from the top.

Of course, if we wrap @racket[quote] around the whole thing, then all of that
reasoning is invalidated, because the argument of a @racket[quote] expression is
not interpreted as a definition or expression.

So ``expression'' doesn't refer to a subset of terms (decidable or not). But
that doesn't mean that it isn't an important concept. Rather, ``expression''
describes an @emph{interpretation} or @emph{intended usage} of a term. Here are
names for the main interpretations that are handled by Racket's macro expander:
@itemlist[

@item{@emph{expression} or @emph{expression term} --- Used in an expression
position, like the test of an @racket[if] or an argument to a function.}

@item{@emph{body term} --- Used as one element of a @racket[lambda] body,
@racket[let] body, etc. A ``body'' is also called an ``internal definition
context''.}

@item{@emph{module-level term} --- Used as one element of a @racket[module] body
or submodule body.}

@item{@emph{top-level term} --- Used at the top level, for example at the REPL
or in a call to @racket[eval].}

]

The word @deftech{form} is used to identify a variant of expression,
module-level term, etc. The concept of ``variant'' usually coincides with the
leading identifier of the term. For example: @racket[if] is an expression form;
@racket[provide] is a module-level form but not a top-level form, but
@racket[require] is allowed both as a module-level form and as a top-level form.

The word @emph{form} can also refer to the entire term, as in ``@racket[(require
racket/list)] is a module-level form''.

The word @emph{definition} refers to a subset of the body forms, roughly. In
fact, we could say that a body term is either an expression or a definition.

@;{
FIXME

So: `(if 1 2 3)` is an expression; it can also be used as a body term; `(require
racket/list)` can be used as a module-level term or a top-level term, but it is
not allowed as an expression. Note that "cond clause" isn't on the list; we'll
come back to that later. (FIXME)

----

FIXME: *valid expressions* -- maybe later. "We can roughly talk about the subset
of terms that are *valid expressions* in a particular context. For example, in
an environment with the standard Racket bindings, `(if 1 2 3)` is a valid
expression but `(lambda)` is not. But ...."

}


@; ------------------------------------------------------------
@section[#:tag "shapes-shapes"]{Shapes}

When we design a macro, the intended interpretation of an argument can be as
important or more important than the set of terms allowed for that argument. To
usefully describe macros and the ways they treat their arguments, we need to
talk about both of these aspects. We'll do that with a semi-formal description
language of @emph{shapes}.

A shape has two aspects:
@itemlist[

@item{the @emph{set of terms} belonging to the shape, and}
@item{the @emph{interpretation} or intended usage of the terms of that shape}

]
Different basic shapes place different degrees of emphasis on these two aspects.

A shape is not the same thing as a syntax pattern, although there is generally a
correspondence between shapes and patterns. In particular, we'll use implement
basic shapes using @emph{syntax classes}. A syntax class check terms for
membership in the shape's set of terms and it can compute attributes related to
the interpretation of the shape. But a syntax class cannot always check every
aspect of a shape's interpretation; for example, a syntax class cannot verify
that we use a term in an expression position in the code that we generate. That
obligation stays with the macro writer.
