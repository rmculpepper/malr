;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse racket/match rackunit syntax/macro-testing))

@(define the-eval (make-malr-eval #:assert? #f))

@(define HtDP (hyperlink "https://htdp.org/" @italic{HtDP}))
@(define HtDP-long @italic{@hyperlink["https://htdp.org/"]{How to Design Programs} (HtDP)})

@; ============================================================
@title[#:tag "intro" #:version ""]{Introduction}

This section introduces the elements of macro design and illustrates these
design elements with simple example macro. It uses the example to introduce some
of Racket's facilities for specifying, implementing, and testing macros.

This guide assumes that you have a basic working knowledge of Racket and
functional programming. @other-doc['(lib "scribblings/guide/guide.scrbl")] is
sufficient for the former, and @HtDP is good for the latter.


@; ------------------------------------------------------------
@section[#:tag "htdm"]{How to Design Macros}

This guide is an attempt to adapt the ideas of @HtDP-long to the design of
macros and languages in Racket. The central idea of @italic{HtDP} is the ``design
recipe''; the kernel of the design recipe consists of the following four steps:

@itemlist[#:style 'numbered

@item{Specify the inputs.}
@item{Write examples that can be turned into tests.}
@item{Choose an implementation strategy.}
@item{Finish the implementation and check it.}

]

@italic{HtDP} instantiates this kernel to teach the foundations of
programming. Its specification language is a semiformal language of types
including set-based reasoning and parametric polymorphism. Its implementation
strategies include structural recursion and case analysis following data type
definitions. It instantiates the implementation language to a series of simple
Scheme-like functional programming languages, and it provides a testing
framework.

Along the way, @italic{HtDP} fills in the design recipe's skeleton with idioms,
tricks, preferences, and limitations of Scheme-like (and ML-like)
mostly-functional programming languages. For example, it demonstrates
abstraction via parametric polymorphism and higher-order functions rather than
OO patterns. To name some of the limitations: it uses lexical scoping; it avoids
reflection (eg, no accessing structure fields by strings); it avoids
@racket[eval]; it treats closures as opaque; it (usually) avoids mutation; and
so on. Once you absorb them, these parts of the programming mental model tend to
be invisible, until you compare with a language that makes different choices.

This guide instantiates the design recipe kernel as follows: It introduces a
specification language called @emph{shapes}, combining features of grammars,
patterns, and types. The implementation strategies are more specialized, but
they are still organized around the shapes of macro inputs. The implementation
language is Racket with @racketmodname[syntax/parse] and some other standard
syntax libraries.

Along the way, it covers some of the idioms and limitations of the programming
model for macros: macros (usually) respect lexical scoping; they must respect
the ``phase'' separation between compile time and run time; they avoid
@racket[eval]; they (usually) treat expressions as opaque; and so on.

@; FIXME: transition

@; FIXME: "and languages"?


@; ------------------------------------------------------------
@section[#:tag "first"]{Designing Your First Macro}

Suppose we wanted a feature, @racket[assert], that takes an expression and
evaluates it, raising an error that includes the expression text if it does not
evaluate to a true value. The result of the @racket[assert] expression itself is
@racket[(void)].

Clearly, @racket[assert] cannot be a function, because a function cannot access
the text of its arguments. It must be a macro.

We can specify the @emph{shape} of @racket[assert] as follows:
@codeblock{
;; (assert Expr) : Expr
}
That is, the @racket[assert] macro takes a single argument, an expression, and a
use of the @racket[assert] macro is an expression.

Here are some examples that illustrate the intended behavior of @racket[assert]:
@examples[#:eval the-eval #:hidden
(define-syntax assert
  (syntax-parser
    [(_ condition:expr)
     #'(unless condition
         (error 'assert "assertion failed: ~s" (quote condition)))]))
]
@examples[#:eval the-eval #:label #f
(define ls '(1 2 3))
(assert (> (length ls) 2))
(eval:error (assert (even? (length ls))))
]

In addition to considering the macro's behavior, it can be useful to consider
what code could be used to implement an example use of the macro. The second
example, for instance, could be implemented by the following code:
@racketblock[
(unless (even? (length ls))
  (error 'assert "assertion failed: (even? (length ls))"))
]
It would be a bit complicated (although possible) for our @racket[assert] macro
to produce this exact code, because it incorporates the argument expression into
a string literal. But there's no need to produce that string literal at compile
time. Here is an equivalent bit of code that produces the same string at run
time instead, with the help of @racket[quote] and @racket[error]'s built-in
formatting capabilities:
@racketblock[
(unless (even? (length ls))
  (error 'assert "assertion failed: ~s" (Quote (even? (length ls)))))
]

@lesson{Don't fixate on the exact code you first write down for the
macro's example expansion. Often, you can change it slightly to make
it easier for the macro to produce.}

@lesson{It's often simpler to produce an expression that does a
computation at run time than to do the computation at compile time.}

That's our implementation strategy for the @racket[assert] macro: we will simply
use @racket[unless], @racket[quote], and @racket[error]. In general, the macro
performs the following transformation:
@racketblock[
(assert _condition)
==>
(unless _condition
  (error 'assert "assertion failed: ~s" (Quote _condition)))
]

Before we define the macro, we must import the machinery we'll use in its
implementation:
@examples[#:eval the-eval #:no-result
(require (for-syntax racket/base syntax/parse))
]
The @racket[for-syntax] modifier indicates that we need these imports to perform
@emph{compile-time} computation --- a macro is implemented by a compile-time
function from syntax to syntax. We need @racketmodname[racket/base] for syntax
templates. We need @racketmodname[syntax/parse] for @racket[syntax-parser],
which is a pattern-matching utility for syntax objects.

Here is the macro definition:

@examples[#:eval the-eval #:no-result
(code:comment "(assert Expr) : Expr")
(define-syntax assert
  (syntax-parser
    [(_ condition:expr)
     (Syntax (unless condition
               (error 'assert "assertion failed: ~e" (Quote condition))))]))
]

Here is an overview of the macro definition:
@itemlist[

@item{The macro is defined using @racket[define-syntax], which takes the macro's
name and a @emph{compile-time} expression for the macro's @emph{transformer}
function. By ``compile-time expression'', I mean that the expression is
evaluated at compile time using the @emph{compile-time environment}, which is
distinct from the normal environment. We initialized the compile-time
environment earlier with @racket[(require (for-syntax racket/base
syntax/parse))].}

@item{The transformer takes a syntax object representing the macro use and
returns a syntax object for the macro's expansion. This transformer is
implemented with @racket[syntax-parser], which takes a sequence of clauses
consisting of a @emph{syntax pattern} and a @emph{result expression}. This
macro's transformer has only one clause.}

@item{The pattern @racket[(_ condition:expr)] says that after the macro name
(typically represented by the wildcard pattern @racket[_]) the macro expects one
expression, representing a ``condition''. The identifier @racket[condition] is a
@emph{syntax pattern variable}; it is @emph{annotated} with the @emph{syntax
class} @racket[expr]. If the clause's pattern matches the macro use, then its
pattern variables are defined and available in @emph{syntax templates} in the
clause's result expression.}

@item{The clause's result expression is a @racket[syntax] expression, which
contains a @emph{syntax template}. It is similar to @racket[quasiquote] except
that pattern variables do not need explicit @racket[unquote]s. (It also
cooperates with ellipses and some other features; we'll talk about them later.)
When the @racket[syntax] expression is evaluated, it produces a syntax object
with the pattern variables in the template replaced with the terms matched from
the macro use. Note that even the occurrence within the @racket[quote] term gets
replaced --- pattern variable substitution happens before the @racket[quote] is
interpreted, so a @racket[quote] in the template is treated like any other
identifier.}

]

Finally, we should test the macro. I'll use @racketmodname[rackunit] for
testing:
@examples[#:eval the-eval #:label #f
(require rackunit)
]
Here @racketmodname[rackunit] is required normally, not @racket[for-syntax],
because I intend to use it to test the behavior of @racket[assert] expressions;
I don't intend to test @racket[assert]'s compile-time transformer function
directly.

@examples[#:eval the-eval #:label #f
(define ls '(1 2 3))
(check-equal? (assert (> (length ls) 1))
              (void))
(check-exn exn:fail?
           (lambda ()
             (assert (even? (length ls)))))
]
What if we want to test uses of @racket[assert] that might result in
compile-time exceptions, like syntax errors? The following does not work:
@examples[#:eval the-eval #:label #f
(eval:error
 (check-exn exn:fail:syntax?
            (lambda ()
              (assert (odd? (length ls)) 'an-extra-argument))))
]
Racket expands and compiles expressions before it evaluates them. The syntax
error is detected and raised at compile time (during expansion), but
@racket[check-exn] does not install its exception handler until run time.

One solution is to use @racket[eval] for this test. This is one of the few
``good'' uses of @racket[eval] in Racket programming. Here's one way to do it:
@examples[#:eval the-eval #:label #f
(define-namespace-anchor anchor)
(check-exn exn:fail:syntax?
           (lambda ()
             (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
               (eval #'(assert (odd? (length ls)) 'an-extra-argument)))))
]

Another solution is to catch the compile-time exception and ``save it'' until
run time. The @racketmodname[syntax/macro-testing] library has a form called
@racket[convert-syntax-error] that does that:
@examples[#:eval the-eval #:label #f
(require syntax/macro-testing)
(check-exn exn:fail:syntax?
           (lambda ()
             (convert-syntax-error
              (assert (odd? (length ls)) 'an-extra-argument))))
]

That completes the design of the @racket[assert] macro. We covered
specification, examples, implementation strategy, implementation, and testing.


@; ------------------------------------------------------------
@section[#:tag "intro-order"]{Expansion Contexts and Expansion Order}

Consider the shape of @racket[assert]:

@codeblock{
;; (assert Expr) : Expr
}

The first @racket[Expr] is for the macro's argument. The second @racket[Expr],
though, says that @racket[assert] forms a new kind of expression. But this also
points to a limitation of macros: @racket[assert] is @emph{only} a new kind of
expression.

Not every term in a program matching a macro's pattern is expanded (that is,
rewritten).  Macros are expanded only in certain positions, called
@emph{expansion contexts}---essentially, contexts where expressions or
definitions may appear. For example, if @racket[assert] is the macro defined
above, then the following occurrences of @racket[assert] do @emph{not} count as
uses of the macro, and they don't get expanded: @itemlist[

@item{@racket[(let ((assert (> 1 2))) 'ok)] --- This occurrence of
@racket[assert] is in a @racket[let]-binding; @racket[assert] is interpreted as
a variable name to bind to the value of @racket[(> 1 2)]. In Racket, names like
@racket[lambda], @racket[if], and @racket[assert] can be shadowed just like
variables can!}

@item{@racket[(cond [assert (odd? 4)] [else 'nope])] --- This is a syntax
error. The @racket[cond] form treats @racket[assert] and @racket[(odd? 4)] as
separate expressions, and the use of @racket[assert] as an expression by itself
is a syntax error (the use does not match @racket[assert]'s pattern).}

@item{@racket['(assert #f)] --- This @racket[assert] occurs as part of a
@racket[quote]d constant.}

]

Note that @racket[let] and @racket[cond] are also macros. So we cannot even tell
whether a term involving @racket[assert] is used as an expression until we
understand the shapes of the surrounding macros. In particular, the Racket macro
expander expands macros in ``outermost-first'' order, in contrast to nested
function calls, which are evaluated ``innermost-first.'' The outermost-first
expansion order is necessary because the macro expander only knows the shapes
(and thus the expansion contexts) of primitive syntactic forms; it must expand
away the outer macros so that it knows what inner terms need to be expanded.


@; ------------------------------------------------------------
@section[#:tag "intro-lexical"]{Proper Lexical Scoping}

Given that @racket[assert] just expands into uses of @racket[unless],
@racket[error], and so on, perhaps we could interfere with its intended behavior
by locally shadowing names it depends on --- @racket[error], for example. But if
we try it, we can see it has no effect:

@examples[#:eval the-eval #:label #f
(eval:error
(let ([error void])
  (assert (even? (length ls)))))
]

The @racket[assert] macro is @emph{properly lexically scoped}, or
@emph{hygienic}. Roughly, that means that references in @racket[assert]'s syntax
template are resolved in the environment where the macro was defined, not the
environment where it is used. This is analogous to the behavior you would get if
@racket[assert] were a function: functions automatically close over their free
variables. In the case of macros, it is syntax objects that contain information
about the syntax's @emph{lexical context}.

In other words, the following ``naive'' code is the wrong explanation for the
expansion of this @racket[assert] example:
@racketblock[
(code:comment "WRONG")
(let ([error void])
  (unless (even? (length ls))
    (error 'assert "assertion failed: ~s" (Quote (even? (length ls))))))
]
Instead, each term introduced by @racket[assert] carries some lexical context
information with it. Here's a better way to think of the expansion:
@racketblock[
(let ([error void])
  (@#,elem{@racket[unless]@superscript{m}} (even? (length ls))
    (@#,elem{@racket[error]@superscript{m}} 'assert "assertion failed: ~s" (@#,elem{@racket[quote]@superscript{m}} (even? (length ls))))))
]
The lexical contexts of @racket[error] and @elem{@racket[error]@superscript{m}}
prevents the use-site local binding of @racket[error] from capturing the
reference @elem{@racket[error]@superscript{m}}.

This example illustrates one half of @emph{hygienic macro expansion}. We'll talk
about the other half in @secref["basic-hygiene2"].


@; ------------------------------------------------------------
@section[#:tag "intro-impl"]{More Implementations of @racket[assert]}

Given that we have all of ``ordinary'' Racket plus several different
macro-defining DSLs available for the implementation of @racket[assert]'s
transformer function, there are many other ways we could implement it. This
section introduces a few of them.

A @racket[(Syntax _template)] expression can be written as @racket[#'_template]
instead. That is, @litchar{#'} is a reader macro for @racket[syntax]. So the
@racket[assert] macro can be defined as follows:
@examples[#:eval the-eval #:no-result
(code:comment "(assert Expr) : Expr")
(define-syntax assert
  (syntax-parser
    [(_ condition:expr)
     #'(unless condition
         (error 'assert "assertion failed: ~e" (Quote condition)))]))
]

The @racket[syntax-parser] form is basically a combination of @racket[lambda]
and @racket[syntax-parse]. So the following definition is equivalent:
@examples[#:eval the-eval #:no-result
(code:comment "(assert Expr) : Expr")
(define-syntax assert
  (lambda (stx)
    (syntax-parse stx
      [(_ condition:expr)
       #'(unless condition
           (error 'assert "assertion failed: ~e" (Quote condition)))])))
]
The @racket[define-syntax] form supports ``function definition'' syntax like
@racket[define] does, so the following is also allowed:
@examples[#:eval the-eval #:no-result
(code:comment "(assert Expr) : Expr")
(define-syntax (assert stx)
  (syntax-parse stx
    [(_ condition:expr)
     #'(unless condition
         (error 'assert "assertion failed: ~e" (Quote condition)))]))
]

A macro's transformer function is, in a sense just an ordinary Racket function,
except that it exists at compile time. When we imported @racket[(for-syntax
racket/base)] earlier, we made the Racket language available at compile time. We
can define the transformer as a separate compile-time function using
@racket[begin-for-syntax]; the definitions it contains are added to the
compile-time environment. Then we can simply use a reference to the function as
the implementation of @racket[assert].
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "assert-transformer : Syntax[(_ Expr)] -> Syntax[Expr]")
  (define (assert-transformer stx)
    (syntax-parse stx
      [(_ condition:expr)
       #'(unless condition
           (error 'assert "assertion failed: ~e" (Quote condition)))])))
(code:comment "(assert Expr) : Expr")
(define-syntax assert assert-transformer)
]

Note: There are two differences between @racket[assert] and
@racket[assert-transformer]. The name @racket[assert] is bound as a @emph{macro}
in the @emph{normal environment} (also called the @emph{run-time environment} or
the @emph{phase-0 environment}), whereas the name @racket[assert-transformer] is
bound as a @emph{variable} in the @emph{compile-time environment} (also called
the @emph{transformer environment} or the @emph{phase-1 environment}). Both of
them are associated with a compile-time value, but @racket[assert-transformer]
is not a macro; if you replace @racket[assert] with @racket[assert-transformer]
in the tests above, they will not even compile. Likewise, you cannot use
@racket[assert] in a compile-time expression, either as a macro or as a
variable. The separation of run-time and compile-time environments is part of
Racket's @emph{phase separation}.


@;{
FIXME: go through expansion step (macro stepper?)
}

In addition to @racketmodname[syntax/parse], Racket also inherits Scheme's older
macro-definition DSLs: @racket[syntax-rules] and @racket[syntax-case], and they
are used in much existing Racket code. Here are versions of @racket[assert]
written using those systems:

@racketblock[
(define-syntax-rule (assert condition)
  (unless condition
    (error 'assert "assertion failed: ~s" (Quote condition))))

(define-syntax assert
  (syntax-rules ()
    [(_ condition)
     (unless condition
       (error 'assert "assertion failed: ~s" (Quote condition)))]))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ condition)
     #'(unless condition
         (error 'assert "assertion failed: ~s" (Quote condition)))]))
]

For a macro as simple as @racket[assert], there isn't much difference. All of
the systems share broadly similar concepts such as syntax patterns and
templates. The @racketmodname[syntax/parse] system evolved out of
@racket[syntax-case]; @racketmodname[syntax/parse] adds a more sophisticated
pattern language and a more expressive way of organizing compile-time syntax
validation and computation.

All of these pattern-matching DSLs are simply aids to writing macros; they
aren't necessary. It's possible to write the macro by directly using the syntax
object API. Here's one version:

@examples[#:eval the-eval #:no-result
(define-syntax assert
  (lambda (stx)
    (define parts (syntax->list stx)) (code:comment "parts : (U (Listof Syntax) #f)")
    (unless (and (list? parts) (= (length parts) 2))
      (raise-syntax-error #f "bad syntax" stx))
    (define condition-stx (cadr parts)) (code:comment "condition-stx : Syntax[Expr]")
    (define code
      (list (quote-syntax unless) condition-stx
            (list (quote-syntax error) (quote-syntax (quote assert))
                  (quote-syntax "assertion failed: ~s")
                  (list (quote-syntax quote) condition-stx))))
    (datum->syntax (quote-syntax here) code)))
]

Briefly, @racket[syntax->list] unwraps a syntax object one level and normalizes
it to a list, if possible (the terms @racket[(a b c)] and @racket[(a ! (b c))],
while both ``syntax lists'', have different syntax object representations). It
is built on top of the primitive operation @racket[syntax-e]. The
@racket[quote-syntax] form is the primitive that creates a syntax object
constant for a term that captures the lexical context of the term itself. The
lexical context can be transferred to a tree using @racket[datum->syntax]; it
wraps pairs, atoms, etc, but it leaves existing syntax objects unchanged.

Here is a variant of the previous definition that uses @racket[quasisyntax]
(reader abbreviation @litchar{#`}) and @racket[unsyntax] (reader abbreviation
@litchar{#,}) to construct the macro's result:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(define-syntax assert
  (lambda (stx)
    (define parts (syntax->list stx)) (code:comment "parts : (U (Listof Syntax) #f)")
    (unless (and (list? parts) (= (length parts) 2))
      (raise-syntax-error #f "bad syntax" stx))
    (define condition-stx (cadr parts)) (code:comment "condition-stx : Syntax[Expr]")
    #`(unless #,condition-stx
        (error 'assert "assertion failed: ~s" (quote #,condition-stx)))))
]

It is not a goal of this guide to introduce you to every bit of machinery that
can be used to implement macros. In general, this guide will stick to the
@racketmodname[syntax/parse] system for macro definitions, and it uses the
@litchar{#'} abbreviation for @racket[syntax] expressions. It will sometimes be
necessary to use the lower-level APIs (such as @racket[syntax->list],
@litchar{#`} and @litchar{#,}) to perform auxiliary computations.

On the other hand, it is a goal of this guide to discuss and compare different
implementation strategies. So the following sections do often present multiple
implementations of the same macro according to different strategies.


@(close-eval the-eval)
