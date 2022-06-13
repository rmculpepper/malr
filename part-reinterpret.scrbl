;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string racket/struct-info syntax/transformer racket/syntax
                     racket/contract racket/list rackunit syntax/parse/experimental/template))

@(define the-eval (make-malr-eval))
@(the-eval '(require racket/match racket/string (for-syntax racket/list)))

@; ============================================================
@title[#:tag "reinterpret"]{Reinterpreting Expressions}

In @secref["basic-expr-uses"] I said that a macro must not look at the contents
of an expression, because expressions are macro-extensible and so there is no
grammar to guide case analysis. On the other hand, there is a grammar for
fully-expanded expressions (see @secref/reference["fully-expanded"]), so it is
feasible to do case analysis on them. This section introduces
@racket[local-expand], which allows a macro to call the macro expander on an
expression or body term. By inspecting the result, a macro can add a new
interpretation to the expression; by transforming the result, it can alter the
standard interpretation of the expression.


@; ------------------------------------------------------------
@section[#:tag "add-interp"]{Adding Interpretations to Expressions}

The standard interpretation of an expression is run-time evaluation, which is a
relatively opaque process. In general, if we want to know something about
@emph{how} an expression will evaluate at run time, we need to make a prediction
based on its fully-expanded code.

For example, let's design the macro @racket[expands-to-quote?], with the
following shape:
@codeblock{
;; (expands-to-quote? Expr) : Expr[Boolean]
}
It produces @racket[#t] if its expression argument expands to a @racket[quote]
form; @racket[#f] otherwise. The expression argument is not evaluated at run
time.

The strategy is this: We use @racket[local-expand] to fully expand the argument
as an expression. Then we can use @racket[syntax-parse] to do case analysis on
the fully-expanded result. In principle, the case analysis is determined by the
shape @shape{FullyExpandedExpr} corresponding to the grammar in
@secref/reference["fully-expanded"], but we can simplify it to two cases, since
we only care whether the expanded expression is a @racket[quote] expression. We
must declare @racket[quote] a literal using @racket[#:literals] (not
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
     (code:comment "ee : Syntax[FullyExpandedExpr]")
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
which determines how it behaves but does not totally determine how it is
implemented.

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
equivalent according to their standard interpretation (that is, they perform the
same effects and produce equivalent values) cause the macro to behave in
different ways.

First, we define a helper to compute the free variables of a fully-expanded
expression.

@racketblock[
(begin-for-syntax
  (code:comment "free-variables : Syntax[FullyExpandedExpr] -> (Listof Id)")
  ELIDED)
]

This function performs case analysis according to the @shape{FullyExpandedExpr}
grammar (see @secref/reference["fully-expanded"]). To distinguish the cases, we
need to declare all of Racket's core syntactic forms as literals. Instead of
using @racket[#:literals] and declaring every one, we can use
#racket[#:literal-sets (kernel-literals)].

Here is the skeleton of a function that processes fully-expanded expressions:
@racketblock[
(code:comment "ee-function : Syntax[FullyExpandedExpr] -> ??")
(define (ee-function ee)
  (syntax-parse ee
    #:literal-sets (kernel-literals)
    [var:id ELIDED]
    [(#%plain-lambda formals body-expr ...+) ELIDED]
    [(case-lambda (formals body-expr ...+) ...) ELIDED]
    [(if test-expr then-expr else-expr) ELIDED]
    [(begin expr ...+) ELIDED]
    [(begin0 result-expr expr ...) ELIDED]
    [(let-values ([(var-id ...) rhs-expr] ...) body-expr ...+) ELIDED]
    [(letrec-values ([(var-id ...) rhs-expr] ...) body-expr ...+) ELIDED]
    [(set! id expr) ELIDED]
    [(quote datum) ELIDED]
    [(quote-syntax datum) ELIDED]
    [(quote-syntax datum #:local) ELIDED]
    [(with-continuation-mark key-expr value-expr body-expr) ELIDED]
    [(#%plain-app expr ...+) ELIDED]
    [(#%top . id) ELIDED]
    [(#%variable-reference id) ELIDED]
    [(#%variable-reference (#%top . id)) ELIDED]
    [(#%variable-reference) ELIDED]))
]

Our @racket[free-variables] function will recur through its argument's
sub-expressions. When it encounters a local variable, it adds it to the output
list. When it processes a binding form like @racket[#%plain-lambda], it
recursively computes the free variables of the body and then removes the
variables bound by the @svar{formals} list. So we need a helper to get the
identifiers of a @svar{formals} (proper or improper) list.

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "formals->ids : Syntax[Formals] -> (Listof Id)")
  (define (formals->ids formals)
    (syntax-parse formals
      [(var:id ...) (datum (var ...))]
      [(var:id ... . rest-var:id) (datum (var ... rest-var))])))
]

We also need a helper to remove bound variables from the free variables of the
body. For example, in the expression @racket[(#%plain-lambda (x) (if x y z))],
the free variables of the body are {@racket[x], @racket[y], @racket[z]}, but the
free variables of the whole expression are {@racket[y], @racket[z]} --- we must
remove the @racket[x] because it is bound by the @racket[#%plain-lambda]
form. We can use @racket[remove*] to do the removal, but we must give it an
appropriate equivalence predicate for identifiers --- @racket[equal?] would not
work, nor would @racket[eq?] or @racket[eqv?]. The most appropriate equivalence
predicate for this task is @racket[bound-identifier=?], which is true when one
of the identifiers used as a binder would capture the other identifier if used
as a reference. That is, @racket[(bound-identifier=? id1 id2)] is true if
@racket[(#%plain-lambda (id1) id2)] is equivalent to the identity function.

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "subtract-ids : (Listof Id) (Listof Id) -> (Listof Id)")
  (define (subtract-ids xs ys)
    (remove* ys xs bound-identifier=?)))
]

Now we can define @racket[free-variables]. It is convenient to define some inner
helper functions that produce lists of identifier with duplicates and then
remove the duplicates at the end. The case analysis follows the skeleton
above. The most important case is the variable (identifier) case; we use
@racket[identifier-binding] to find out how the variable is bound, and we ignore
module-bound variables. For binding forms, we process the body and then remove
the bound variables from the result. For other forms, we simply recur on the
subexpressions.

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "free-variables : Syntax[FullyExpandedExpr] -> (Listof Identifier)")
  (code:comment "Return list of free variables, excluding module-level bindings.")
  (define (free-variables ee)
    (code:comment "loop : Syntax[FullyExpandedExpr] -> (Listof Identifier)")
    (code:comment "May contain duplicates.")
    (define (loop ee)
      (syntax-parse ee
        #:literal-sets (kernel-literals)
        [var:id
         (define b (identifier-binding #'var))
         (cond [(list? b) (code:comment "module-level binding")
                null]
               [else      (code:comment "lexical or top-level (if outside module)")
                (list #'var)])]
        [(#%plain-lambda formals body-expr ...+)
         (subtract-ids (loop* #'(body-expr ...))
                       (formals->ids #'formals))]
        [(case-lambda (formals body-expr ...+) ...)
         (append*
          (for/list ([formals (in-list (datum (formals ...)))]
                     [bodies (in-list (datum ((body-expr ...) ...)))])
            (subtract-ids (loop* (datum->syntax #f bodies))
                          (formals->ids formals))))]
        [(if test-expr then-expr else-expr)
         (loop* #'(test-expr then-expr else-expr))]
        [(begin expr ...+)
         (loop* #'(expr ...))]
        [(begin0 result-expr expr ...)
         (loop* #'(result-expr expr ...))]
        [(let-values ([(var-id ...) rhs-expr] ...) body-expr ...+)
         (append (loop* #'(rhs-expr ...))
                 (subtract-ids (loop* #'(body-expr ...))
                               (datum (var-id ... ...))))]
        [(letrec-values ([(var-id ...) rhs-expr] ...) body-expr ...+)
         (subtract-ids (loop* #'(rhs-expr ... body-expr ...))
                       (datum (var-id ... ...)))]
        [(set! id expr)
         (cons #'id (loop #'expr))]
        [(quote datum) null]
        [(quote-syntax datum) null]
        [(quote-syntax datum #:local) null]
        [(with-continuation-mark key-expr value-expr body-expr)
         (loop* #'(key-expr value-expr body-expr))]
        [(#%plain-app expr ...+)
         (loop* #'(expr ...))]
        [(#%top . id)
         (loop #'id)]
        [(#%variable-reference id) null]
        [(#%variable-reference (#%top . id)) null]
        [(#%variable-reference) null]))
    (code:comment "loop* : Syntax[(FullyExpandedExpr ...)] -> (Listof Identifier)")
    (code:comment "May contain duplicates.")
    (define (loop* ees)
      (append* (map loop (syntax->list ees))))
    (code:comment "----")
    (remove-duplicates (loop ee) bound-identifier=?)))
]

Before continuing on to update @racket[assert], we can write a little wrapper
macro @racket[FV] to help test the compile-time @racket[free-variables]
function. The @racket[FV] macro takes an expression, fully expands it, computes
the free variables, and then @racket[quote]s them. To perform full expansion of
an expression, we call @racket[local-expand] with the expression,
@racket['expression] as the context argument, and an empty @emph{stop list}. The
@racket[quote] turns the compile-time list of identifiers into a run-time list
of symbols.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(FV Expr) : Expr")
(define-syntax FV
  (syntax-parser
    [(_ e:expr)
     (define ee (local-expand #'e 'expression null))
     (define fvs (free-variables ee))
     #`(Quote #,fvs)]))
]

Here are some basic examples:

@examples[#:eval the-eval #:label #f #:escape UNQUOTE
(let ([x 1] [y 2])
  (FV (lambda (z) (+ x y z))))
(let ([x 1])
  (FV (let ([y x]) (+ x y))))
]

Now we are finally ready to return to @racket[assert]. We expand the condition
expression and get its free variables. If the condition fails we call a helper
function to raise the error, passing it an association list containing each
variable's symbolic name and its value.

We want to evaluate the condition expression at run time, but if we use
@racket[condition] in the template then we are effectively using
@racket[condition] twice --- it's essentially the same as duplicating the
expression. Instead, we should use the expanded expression, @racket[econdition].
This is one of the great strengths of the macro approach to metaprogramming:
expanding an expression results in another valid expression, not some different
kind of value. The result has a known, fixed structure
(@shape{FullyExpandedExpr}) that allows us to analyze it, but it can also be
treated as a simple @shape{Expr} and fed back to the macro expander.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(define-syntax assert
  (syntax-parser
    [(_ condition:expr)
     (define econdition (local-expand #'condition 'expression null))
     (define vars (free-variables econdition))
     (with-syntax ([(var ...) vars])
       #`(unless #,econdition
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


@exercise[#:tag "reinterp-fv-env"]{There is another way to handle binding
forms in the computation of free variables. Instead of computing all of the free
variables of the body and then removing the bound variables, add a second
argument to the function that holds a list of all of the local variables
currently in scope, and in the variable case only add local variables that are
not in that list. That is, the recursive function should have the following
type:

@codeblock{
;; free-vars-except : Syntax[FullyExpandedExpr] (Listof Id) -> (Listof Id)
}}

@exercise[#:tag "reinterp-fv-idtable"]{The @racketmodname[syntax/id-table]
library provides mutable and immutable dictionaries specialized to identifier
keys, using either the @racket[free-identifier=?] or @racket[bound-identifier=?]
equivalence predicate. Rewrite your solution to @exercise-ref["reinterp-fv-env"]
to use identifier tables instead of lists.}

@; @exercise[#:tag "reinterp-fv-stxclass"]{...}

@exercise[#:stars 1 #:tag "reinterp-fv-stxclass"]{Redefine
@racket[free-variables] using a helper syntax class with an @racket[fvs]
attribute instead of a recursive helper function.

@racketblock[
(begin-for-syntax
  (define-syntax-class eexpr
    #:attributes (fvs) (code:comment "(Listof Id), may contain duplicates")
    ELIDED)

  (code:comment "free-variables : Syntax[FullyExpandedExpr] -> (Listof Id)")
  (define (free-variables ee)
    (syntax-parse ee
      [ee:eexpr (remove-duplicates (datum ee.fvs) bound-identifier=?)])))
]}

@exercise[#:stars 2 #:tag "reinterp-letifneeded"]{Implement the macro
@racket[let-if-needed], which has the following shape:

@codeblock{
;; (let-if-needed ([x:Id Expr] ...) Expr{x...}) : Expr
}

The macro acts like @racket[let], except that each right-hand side is evaluated
only if there is a reference to the corresponding variable in the body. This is
not the same as @racket[let-lazy]: if there is a reference to a variable in the
body, the variable's right-hand side is evaluated and bound to the variable,
regardless of whether the variable reference is ever evaluated. For example:
@margin-note{FIXME: @racket[let-lazy] exercise}

@racketblock[
(define outer 'no)
(let-if-needed ([x (begin (set! outer 'yes) 123)]
                [y (error 'nope)])
  (if #t outer x))
(code:comment "expect 'yes")
]

Note: it is incorrect to simply call @racket[local-expand] on the body
expression, because that expression has unsatisfied scoping obligations. The
shape @shape{Expr{x...}} represents a promise that the expression will be placed
in the scope of variable bindings for the @shape{x} identifiers by the time the
macro expander visits it. Consider the following example:

@racketblock[
(let-syntax ([x (lambda (stx) (raise-syntax-error #f "bad syntax" stx))])
  (let-if-needed ([x (for/sum ([i 1000]) (* i i))])
    x))
]

If the @racket[let-if-needed] macro just calls @racket[local-expand] on its body
expression, then the @racket[x] reference points to the ever-failing outer
macro. That would be a scoping error in @racket[let-if-needed].}


@; DrRacket draws no arrows for disappeared exprs. Fix it.


@; ------------------------------------------------------------
@section[#:tag "change-interp"]{Changing the Interpretation of Expressions}

A macro can also @emph{change} the interpretation of an expression by
@racket[local-expand]ing it and transforming the result.

Let's design a @racket[trace] macro that takes an expression and evaluates
it, except that all function calls performed within the expression are traced
with printouts. The macro's shape is

@codeblock{
;; (trace Expr) : Expr
}

The broad implementation strategy is to fully expand the expression argument and
then traverse it, rewriting all function applications to emit tracing
output. There are a few different ways of implementing the rewriting process. In
the following subsections we explore three of them:

@itemlist[

@item{monolithic rewrite function}
@item{monolithic rewrite function with metafunctions}
@item{trampoline-style helper macro}

]

@; ----------------------------------------
@subsection[#:tag "monolithic"]{Monolithic Rewrite Function}

One way of implementing the tracing instrumentation is as a @emph{monolithic}
compile-time helper function. This approach is monolithic in the sense that the
entire rewriting process is completed during the expansion of the @racket[trace]
macro. The skeleton of the rewriting helper function is based on the grammar of
@shape{FullyExpandedExpr}; it is same as the skeleton of the analysis helper
function from @secref["free-vars"].

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (code:comment "trace-transform : Syntax[FullyExpandedExpr] -> Syntax[Expr]")
  (define (trace-transform ee)
    (code:comment "loop* : (Listof Syntax[FullyExpandedExpr]) -> (Listof Syntax[Expr])")
    (define (loop* ees) (map loop ees))
    (code:comment "loop : Syntax[FullyExpandedExpr] -> Syntax[Expr]")
    (define (loop ee)
      (syntax-parse ee
        #:literal-sets (kernel-literals)
        (code:comment "Transform")
        [(#%plain-app expr_ ...+)
         (with-syntax ([(expr* ...) (loop* (datum (expr_ ...)))])
           #'(trace-app expr* ...))]
        (code:comment "Recur")
        [(#%plain-lambda formals body-expr ...+)
         (with-syntax ([(body-expr* ...) (loop* (datum (body-expr ...)))])
           #'(#%plain-lambda formals body-expr* ...))]
        [(case-lambda (formals body-expr ...+) ...)
         (with-syntax ([((body-expr* ...) ...)
                        (map loop* (datum ((body-expr ...) ...)))])
           #'(case-lambda (formals body-expr* ...) ...))]
        [(let-values ([(var-id ...) rhs-expr] ...) body-expr ...+)
         (with-syntax ([(rhs-expr* ...) (loop* (datum (rhs-expr ...)))]
                       [(body-expr* ...) (loop* (datum (body-expr ...)))])
           #'(let-values ([(var-id ...) rhs-expr*] ...) body-expr* ...))]
        [(letrec-values ([(var-id ...) rhs-expr] ...) body-expr ...+)
         (with-syntax ([(rhs-expr* ...) (loop* (datum (rhs-expr ...)))]
                       [(body-expr* ...) (loop* (datum (body-expr ...)))])
           #'(letrec-values ([(var-id ...) rhs-expr*] ...) body-expr* ...))]
        [(if test-expr then-expr else-expr)
         (with-syntax ([(test-expr* then-expr* else-expr*)
                        (loop* (datum (test-expr then-expr else-expr)))])
           #'(if test-expr* then-expr* else-expr*))]
        [(begin expr_ ...+)
         (with-syntax ([(expr* ...) (loop* (datum (expr_ ...)))])
           #'(begin expr* ...))]
        [(begin0 result-expr expr_ ...)
         (with-syntax ([(result-expr* expr* ...) (loop* (datum (result-expr expr_ ...)))])
           #'(begin0 result-expr* expr* ...))]
        [(set! id expr)
         (with-syntax ([expr* (loop #'expr)])
           #'(set! id expr*))]
        [(with-continuation-mark key-expr value-expr body-expr)
         (with-syntax ([(key-expr* value-expr* body-expr*)
                        (loop* (datum (key-expr value-expr body-expr)))])
           #'(with-continuation-mark key-expr* value-expr* body-expr*))]
        (code:comment "Base, includes variable, quote, quote-syntax, etc")
        [e #'e]))
    ;; ----
    (loop ee)))
]

@; FIXME: with-syntax vs #:with vs { #:cut #:with }

Here is the @racket[trace-app] helper. It uses @racket[call-with-values] to
handle procedures that may produce multiple values.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "trace-app : (X ... -> Y ...) X ... -> Y ...")
(define (trace-app f . args)
  (define (format-vals vs)
    (string-join (map (lambda (v) (format "~e" v)) vs) ", "))
  (printf ">> call ~a with ~a\n"
          (or (object-name f) f)
          (format-vals args))
  (call-with-values (lambda () (apply f args))
                    (lambda results
                      (printf "<< received ~a\n" (format-vals results))
                      (apply values results))))
]

Here is the macro. Like before, it uses @racket[local-expand] in
@racket['expression] mode with an empty stop list to fully expand the
expression. Then it calls the transformation helper function to implement the
tracing instrumentation.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(define-syntax trace
  (syntax-parser
    [(_ e:expr)
     (trace-transform
      (local-expand #'e 'expression null))]))
]

Here are some examples:

@examples[#:eval the-eval #:label #f
(define (double n) (+ n n))
(trace
 (list (+ (double 2) 3)
       (+ 4 5)))
(trace
 (let ([square (lambda (n) (* n n))])
   (double (square 5))))
]

One flaw of this implementation is that it fails to preserve much of the input
expanded expression. It replaces the original Racket core form identifiers (eg,
@racket[#%plain-lambda], @racket[if]) in the expanded expression with equivalent
identifiers from the function's syntax templates. It replaces the source
location information and syntax properties of subexpressions with the locations
and (empty) properties of the function's syntax templates. Usually, replacing
the source locations and syntax properties will not change the main behavior of
the program, but it can lead to worse error backtraces (which depend on source
locations) and worse programming environment interactions (for example, DrRacket
draws binding arrows based on information stored in syntax properties). It is
also possible for custom macros and languages to store semantically significant
information in syntax properties.

@; Advice: only place syntax properties on expr nodes and (maybe?) binders.

The first problem can be solved by reusing the original core form identifier in
the result template. One way of doing that is by using the @racket[~and] pattern
form. For example, the @racket[#%plain-lambda] clause could be rewritten as
follows:

@racketblock[
    [((~and kw #%plain-lambda) formals body-expr ...+)
     (with-syntax ([(body-expr* ...) (loop* (datum (body-expr ...)))])
       #'(kw formals body-expr* ...))]
]

As a convenience, identifiers declared as literals (including indirectly via
literal sets) can be treated as syntax classes, so you can use the ``colon''
notation instead:

@racketblock[
    [(kw:#%plain-lambda formals body-expr ...+)
     (with-syntax ([(body-expr* ...) (loop* (datum (body-expr ...)))])
       #'(kw formals body-expr* ...))]
]

The second problem can be fixed by a local helper function that ``repairs'' the
syntax produced by each rewriting clause by restoring the lexical context,
source location, and syntax properties of the original expression. Only the
outer layer of ``syntax wrapping'' should be replaced, and only on terms where
the outer layer is freshly produced by a template within the
@racket[trace-transform] function --- an expression that is left unchanged, like
a @racket[quote] expression or a variable reference, should not be
repaired. Here is a sketch of @racket[trace-transform] with both fixes:

@racketblock[
(begin-for-syntax
  (code:comment "trace-transform : Syntax[FullyExpandedExpr] -> Syntax[Expr]")
  (define (trace-transform ee)
    ELIDED
    (code:comment "loop : Syntax[FullyExpandedExpr] -> Syntax[Expr]")
    (define (loop ee)
      (code:comment "repair : Syntax -> Syntax")
      (code:comment "PRE: The outer layer of the argument was produced by a syntax template here.")
      (define (repair stx)
        (datum->syntax ee (syntax-e stx) ee ee))
      (syntax-parse ee
        #:literal-sets (kernel-literals)
        (code:comment "Transform")
        [(kw:#%plain-app expr_ ...+)
         (with-syntax ([(expr* ...) (loop* (datum (expr_ ...)))])
           (repair #'(kw trace-app expr* ...)))]
        (code:comment "Recur")
        [(kw:#%plain-lambda formals body-expr ...+)
         (with-syntax ([(body-expr* ...) (loop* (datum (body-expr ...)))])
           (repair #'(kw formals body-expr* ...)))]
        [(kw:case-lambda (formals body-expr ...+) ...)
         (with-syntax ([((body-expr* ...) ...)
                        (map loop* (datum ((body-expr ...) ...)))])
           (repair #'(kw (formals body-expr* ...) ...)))]
        ELIDED
        (code:comment "Base, includes variable, quote, quote-syntax, etc")
        (code:comment "Note: don't call repair!")
        [e #'e]))
      ELIDED))
]


@; ----------------------------------------
@subsection[#:tag "monolithic+metafun"]{Monolithic Rewriter with Metafunctions}

The definition of @racket[trace-transform] is a bit clunky due to the mismatch
between ellipses in syntax patterns and explicit @racket[map] (encapsulated in
the @racket[loop*] inner helper function), and the need for @racket[with-syntax]
and @racket[datum] to bridge these two modes of computation. It would be more
convenient if we could somehow use ellipses around the recursive function
calls. But that is not possible with ordinary functions. The @racket[unsyntax]
escape for @racket[quasisyntax] templates does not cooperate with ellipses,
either. It is possible, however, by wrapping the function as a @deftech{syntax
template metafunction}.

Metafunctions are not macros. They are run by the @racket[syntax] (or
@racket[quasisyntax]) form during the construction of the syntax object from the
template. Roughly, @racket[syntax] constructs a syntax object normally from the
template; then, for each metafunction application within the resulting term,
starting with the innermost, it runs the metafunction and replaces the
metafunction application with the syntax it produces. Ellipses can cause one
metafunction application in the template to produce multiple metafunction
applications in the intermediate syntax object. (In actuality, there is no
intermediate syntax object; the evaluation of the metafunctions is interleaved
with the construction of the syntax object.)

We can use a local metafunction to simplify the recursive calls in
@racket[trace-transform] as follows. We define @racket[T] as a metafunction that
expects one argument and simply calls the inner @racket[loop] with that
argument. We no longer need the @racket[loop*] helper. We could also eliminate
@racket[loop] and make @racket[trace-transform] directly recursive instead, but
I've chosen not to do that here.

@racketblock[
(begin-for-syntax
  (code:comment "trace-transform : Syntax[FullyExpandedExpr] -> Syntax[Expr]")
  (define (trace-transform ee)
    (define-template-metafunction T
      (syntax-parser
        [(_ e) (loop #'e)]))
    (code:comment "loop : Syntax[FullyExpandedExpr] -> Syntax[Expr]")
    (define (loop ee)
      (code:comment "repair : Syntax -> Syntax")
      (code:comment "PRE: The outer layer of the argument was produced by a syntax template here.")
      (define (repair stx)
        (datum->syntax ee (syntax-e stx) ee ee))
      (syntax-parse ee
        #:literal-sets (kernel-literals)
        (code:comment "Transform")
        [(kw:#%plain-app expr_ ...+)
         (repair #'(kw trace-app (T expr_) ...))]
        (code:comment "Recur")
        [(kw:#%plain-lambda formals body-expr ...+)
         (repair #'(kw formals (T body-expr) ...))]
        [(kw:case-lambda (formals body-expr ...+) ...)
         (repair #'(kw (formals (T body-expr) ...) ...))]
        ELIDED))
    (code:comment "----")
    (loop ee)))
]

@exercise[#:stars 1 #:tag "reinterp-metafun-env"]{If the recursive function
takes only syntax arguments, it is straightforward to write a metafunction
adapter. If it takes additional non-syntax arguments, however, such as an
identifier table, then the additional argument cannot be passed through the
syntax template. One solution is to compute the additional recursive argument
and define a local metafunction specialized to that argument value. Here is a
sketch of the @racket[#%plain-lambda] clause for a variant of @racket[loop] that
takes an extra non-syntax @racket[env] argument:

@racketblock[
      [(kw:#%plain-lambda formals body-expr ...+)
       (define env* (extend-env-with-formals env #'formals))
       (define-template-metafunction LT
         (syntax-parser
           [(_ e) (loop #'e env*)]))
       (repair #'(kw formals (LT body-expr) ...))]
]

Modify @racket[trace-transform] so that references and updates (@racket[set!])
to variables are also traced, but only if the variable is bound within the
expression given to @racket[trace]. Use an immutable bound-identifier table
(@racketmodname[syntax/id-table]) or set (@racketmodname[syntax/id-set]) to keep
track of the set of variables to trace.}


@; ----------------------------------------
@subsection[#:tag "trampoline"]{Trampoline-Style Rewrite Macro}

The monolithic solutions both perform the rewriting of the expanded expression
all at once, within a single macro step. The second solution used syntax
template metafunctions, which have the appearance of function or macro calls
around the subexpressions but in fact get applied when the syntax template is
evaluated. But why not use a macro instead of a metafunction? That leads us to
another implementation strategy: a recursive macro.

This implementation is an example of @deftech{trampoline style}. The expander
calls the rewrite macro's transformer function, but instead of completing the
entire rewrite in a single macro step, the macro only does a fixed amount of
work (typically, processes one level of the expanded expression), leaves
instructions to continue the process in the form of recursive macro calls, and
then returns to the expander. The expander continues traversing the term, and
when it hits one of the recursive macro calls, it transfers control back to the
macro to resume the rewriting. Control keeps bouncing from the expander to the
macro and then falling back from the macro to the expander until the rewriting
is complete.

The idea of the trampoline is fundamental to macro programming. Every macro is
implemented in trampoline style, if trampoline style is interpreted broadly
enough. A macro does not complete the compilation of a term itself; it is not
capable, because the macro API does not provide access to the necessary
support. Instead, it merely transforms the term into a simpler term using Racket
core forms, other macros, and recursive calls to itself as helpers. Then control
returns to the expander, which completes the transformation and then calls the
compiler on the fully-expanded program.

Tangent: Historically, ``trampoline'' referred to a technique for implementing
proper tail calls for Scheme on platforms that don't natively support tail calls
(for example, compiling to C or the JVM). To implement a tail call, instead of
performing a @emph{call}, the current function performs a @emph{return} (setting
a flag or returning a special indicator value), and the @emph{trampoline}
detects that case and performs the call on its behalf. Since the call is made
from the trampoline, the previous function's stack frame is cleared and the
stack does not grow. More generally, I use the term ``trampoline'' to refer to
any situation where a bit of code is unable or unwilling to perform an action
directly, so it returns a request to a more capable layer --- the trampoline ---
which performs the action on its behalf. In Scheme compiled to C, a function is
unable to perform a function call without growing the stack. In Javascript, a
computation cannot run for a long period without risking interruption, so it
uses the @tt{setTimeout} trampoline with a resumption callback. In Haskell, the
pure language cannot directly perform IO and other side effects, so it returns
an IO request value to the Haskell-OS bridge, which interprets the reqeust,
performs the operation, and resumes evaluation of the pure continuation.

Here is a sketch of the trampoline-style solution. Note that the
@racket[trace-transform] helper macro takes a @shape{FullyExpandedExpr}
argument.

@RACKETBLOCK[
(code:comment "(trace Expr) : Expr")
(define-syntax trace
  (syntax-parser
    [(_ e:expr)
     #`(trace-transform #,(local-expand #'e 'expression null))]))

(code:comment "(trace-transform FullyExpandedExpr) : Expr")
(define-syntax trace-transform
  (syntax-parser
    #:literal-sets (kernel-literals)
    [(_ ee:expr)
     (code:comment "repair : Syntax -> Syntax")
     (code:comment "PRE: The outer layer of the argument was produced by a syntax template here.")
     (define (repair stx) (datum->syntax ee (syntax-e stx) ee ee))
     (syntax-parse #'ee
       #:literal-sets (kernel-literals)
       (code:comment "Transform")
       [(_ (kw:#%plain-app expr_ ...+))
        (repair #'(kw trace-app (trace-transform expr) ...))]
       (code:comment "Recur")
       [(kw:#%plain-lambda formals body-expr ...+)
        (repair #'(kw formals (trace-transform body-expr) ...))]
       [(kw:case-lambda (formals body-expr ...+) ...)
        (repair #'(kw (formals (trace-transform body-expr) ...) ...))]
       ELIDED
       (code:comment "Base, includes variable, quote, quote-syntax, etc")
       (code:comment "Note: don't call repair!")
       [e #'e])]))
]

One advantage of the trampoline style over the monolithic style is that it
breaks the rewriting process into multiple small steps, and each step is visible
to debugging tools such as the Macro Stepper. A disadvantage is that there is no
good way to pass additional non-syntax-valued parameters to the recursive calls
(but there are a few bad ways to do it).

@exercise[#:tag "reinterp-trampoline"]{Complete the implementation of
@racket[trace] using the trampoline-style helper macro
@racket[trace-transform].}

@;{ Con: no good way to do threaded computation (eg, state, nonlocal analysis...) }

@(close-eval the-eval)
