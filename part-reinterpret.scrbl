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
@(the-eval '(require racket/match racket/string))

@; ============================================================
@title[#:tag "reinterpret"]{Reinterpreting Racket}

In @secref["basic-expr-uses"] I said that a macro must not look at the contents
of an expression, because expressions are macro-extensible and so there is no
grammar to guide case analysis. On the other hand, there is a grammar for
fully-expanded expressions (see @secref/reference["fully-expanded"]), so it is
feasible to do case analysis on them. This section introduces
@racket[local-expand], which allows a macro to call the macro expander on an
expression or body term and expand it either fully or partially. By inspecting
the result, a macro can add a new interpretation to the expression; by
transforming the result, it can alter the standard interpretation of the
expression.


@; ------------------------------------------------------------
@section[#:tag "add-interpret"]{Adding Interpretations to Expressions}

The standard interpretation of an expression is run-time evaluation, which is a
relatively opaque process. In general, if we want to know something about how an
expression will evaluate at run time, we need to make a prediction based on its
fully-expanded code.

For example, consider the macro @racket[expands-to-quote?], with the following
shape:
@codeblock{
;; (expands-to-quote? Expr) : Expr[Boolean]
}
which produces @racket[#t] if its expression argument expands to a
@racket[quote] form; @racket[#f] otherwise. The expression argument is not
evaluated at run time.

The strategy is this: We use @racket[local-expand] to fully expand the argument
as an expression. Then we can use @racket[syntax-parse] to do case analysis on
the result. In principle, the case analysis is determined by the grammar in
@secref/reference["fully-expanded"], but since we only care whether it is a
@racket[quote] expression, we can simplify it to two cases. We must declare
@racket[quote] a literal using @racket[#:literals] (not
@racket[#:datum-literals]), because we want to recognize references to Racket's
@racket[quote] syntax, whether the references are spelled @litchar{quote} or
not.

Here is the implementation:

@examples[#:eval the-eval #:no-result
(code:comment "(expands-to-quote? Expr) : Expr[Boolean]")
(define-syntax expands-to-quote?
  (syntax-parser
    [(_ e:expr)
     (define ee (local-expand #'e 'expression null))
     (syntax-parse ee
       #:literals (Quote)
       [(Quote _) #'#t]
       [_ #'#f])]))
]

Here are some examples:

@examples[#:eval the-eval #:label #f
(expands-to-quote? (quote abc))
(expands-to-quote? 6)
(expands-to-quote? (quasiquote (1 2 3)))
(expands-to-quote? (quasiquote (1 2 ,3)))
(expands-to-quote? (+ 1 2 3))
]

Note that @racket[(expands-to-quote? 6)] is true because the macro expander
handles a number term by implicitly wrapping it with a @racket[#%datum] macro
call based on the lexical context of the number term, and Racket's
@racket[#%datum] macro expands into an explicit @racket[quote]
expression. Boolean terms, string terms, and other ``self-quoting'' terms are
handled the same way. This explains why mishandling @shape{Datum} terms can
result in an error about a missing @racket[#%datum] binding (see
@exercise-ref["unh-implicit"]).

Note also that @racket[(expands-to-quote? (+ 1 2 3))] is false, because the
argument is an application. On the other hand, this code @emph{compiles} to a
constant, because Racket's compiler performs constant folding. So remember that
looking at fully-expanded code tells you how a program is @emph{expressed},
which constrains how it behaves but does not totally determine how it behaves.

Another macro we might want is @racket[allocates-closure?], which is true if the
evaluation of its expression argument would cause the allocation of a closure
and false otherwise. This predicate cannot be implemented by a macro,
though. One implementation strategy would be to traverse the expression's
fully-expanded code looking for occurrences of @racket[lambda] and
@racket[case-lambda]. This would be fooled by an expression like
@racket[((lambda (x) (add1 x)) 3)], which does not allocate a closure at run
time because the compiler optimizes it into a constant. On the other hand, it
would miss the (probable) closure allocation in the following example:
@racketblock[
(let ([const (lambda (c) (lambda ignore c))])
  (allocates-closure? (const 123)))
]
Macros only have direct control over their argument terms. They interact with
their contexts only in limited ways, such as through static bindings in the
environment (see @secref["static-shape"]). In this example,
@racket[allocates-closure?] cannot see the definition of @racket[const] in the
outer scope, and so it has no information about its behavior.

@lesson{Macros are local transformations. They cannot directly see or affect
code outside of the macro call itself, except in a few limited ways such as
communication through the environment.}


@;{ FIXME: Whole-module analysis is beyond the capability of macros; it
    requires a @emph{language}. (But library code imposes similar limitations.)}

@; ------------------------------------------------------------
@section[#:tag "free-vars"]{Free Variables}

Recall the @racket[assert] macro from @secref["first"]. When the assertion
fails, it prints the text of the assertion expression, but the expression
generally depends on the values of local variables, and those are missing from
the error message. Let's add them!

That is, we are adding an interpretation to the expression argument of
@racket[assert]. The behavior of the macro depends not only on the value that
the expression produces, but also the free variables used to express it.

@margin-note{Does this strictly follow the definition of referential
transparency?}

As a consequence, the argument to @racket[assert] is no longer a
@emph{referentially transparent context}, because expressions that are
equivalent according to their standard interpretation (that is, the perform the
same effects and produce equivalent values) cause the macro to behave in
different ways.

@examples[#:eval the-eval #:no-result
(require (for-syntax syntax/free-vars))
(define-syntax assert
  (syntax-parser
    [(_ condition:expr)
     (define econdition (local-expand #'condition 'expression null))
     (define vars (free-vars econdition))
     (with-syntax ([(var ...) vars])
       #'(unless condition
           (assert-error (Quote condition)
                         (list (list (Quote var) var) ...))))]))
(code:comment "assert-error : Any (Listof (list Symbol Any)) -> Void")
(define (assert-error condition-expr env)
  (error 'assert "assertion failed: ~s~a" condition-expr
         (string-join
          (for/list ([entry (in-list env)])
            (format "\n  ~a: ~e" (car entry) (cadr entry)))
          "")))
]

Here is an example:

@examples[#:eval the-eval #:label #f
(eval:error
(for/sum ([i (in-range 1 10)])
  (define i^2 (* i i))
  (assert (not (zero? (modulo i^2 18))))
  i^2))
]

@;{ FIXME: section on implementing free-vars using case-analysis, lists,
    id-binding, free-id=? to remove duplicates? }

@; => id-tables?




@; FIXME: (let-lazy ([x:Id Expr] ...) Expr{x...}) -- use promise, identifier macros
@; FIXME: (let-if-needed ([x:Id Expr] ...) Expr{x...}) : Expr

@; FIXME: Cooperation: trampoline-style macros, eg printing-module-begin?

@; FIXME: "global reinterpretation" => languages!

@(close-eval the-eval)
