;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/port rackunit))

@(define the-eval (make-base-eval))
@(the-eval '(require (only-in racket/base [quote Quote] [syntax Syntax])
                     rackunit syntax/macro-testing racket/port
                     (for-syntax racket/base racket/syntax syntax/parse syntax/datum
                                 (only-in racket/base [quote Quote] [syntax Syntax]))))

@; ============================================================
@title[#:tag "multi-shapes" #:version ""]{Multi-Term Shapes}

This section introduces ``multi-term'' shapes, used to describe syntactic
elements like keyword arguments.


@; ------------------------------------------------------------
@section[#:tag "multi-terms"]{Shapes for Multiple Terms}

In Racket, the syntax of a ``keyword argument'' to a function does not consist
of a single term; it consists of two terms, a keyword followed by the argument
term. That is, the logical grouping structure does not correspond with the term
structure. The syntax of macros generally follows the same idiom: a macro
keyword argument consists of the keyword and zero or more argument terms,
depending on the keyword. For example, the @racket[#:attributes] keyword used by
@racket[define-syntax-class] takes one argument (a list of attributes); and the
@racket[#:with] keyword takes two (a syntax pattern and an expression).

We can define shapes that stand for multiple terms like this:
@codeblock{
;; AttributesClause ::= #:attributes (Id ...)
;; WithClause       ::= #:with SyntaxPattern Expr
}
Multi-term shapes are represented by @emph{splicing syntax classes}, which
encapsulate @emph{head syntax patterns} (so called because they match some
variable-length ``head'' of the list term).

Let's extend @racket[my-cond] with support for a @racket[#:do] clause that has a
single @shape{Body} argument. That will allow us to include definitions between
tests. Here's an example:
@racketblock[
(define ls '((a 1) (b 2) (c 3)))
(define entry (assoc 'b ls))
(my-cond [(not entry) (error "not found")]
         #:do (define value (cadr entry))
         [(even? value) 'even]
         [(odd? value) 'odd])
]

Here is the revised definition of @shape{CondClause}, which is now a multi-term
shape:
@codeblock{
;; CondClause ::=
;; | [Expr Expr]
;; | [Expr #:apply Expr]
;; | #:do Body
}

Here is the corresponding syntax class, including only the patterns:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-splicing-syntax-class cond-clause
    #:attributes ()
    (pattern [condition:expr result:expr])
    (pattern [condition:expr #:apply get-result:expr])
    (pattern (~seq #:do body:expr))))
]
We must declare @racket[my-cond] using @racket[define-splicing-syntax-class],
and we must use @racket[~seq] to wrap multiple-term patterns.

What interface can we give to the syntax class, and how do we implement the
macro? Let's review the implementations of @racket[my-cond] from
@secref["enum-shapes-def"]:
@itemlist[

@item{The approach of redoing the case analysis from @secref["enum-empty"] would
also still work.}

@item{The approach from @secref["enum-meaning"] no longer works, because
@racket[#:do] clauses are not a special case of @racket[#:apply] clauses.}

@item{The failure-continuation approach from @secref["enum-behavior"] no longer
works, because the scope of definitions within @racket[#:do] clauses should
cover the rest of the clauses, but the failure continuation is received as a
closure value, and there's no way to affect its environment.}

@item{The code generator approach from @secref["enum-codegen"] would still work,
since the code generator for the @racket[#:do] clause can put the expression
representing the rest of the clauses in the scope of the @racket[#:do]-clause's
definitions.}

@item{The AST approach from @secref["enum-ast"] would still work. We would need
to update the AST datatype with a new variant and update the macro's case
analysis to handle it.}

]

This is a good summary of how robust each of these strategies is to changes in
the shape.

@; ----------------------------------------
@subsection[#:tag "multi-redo-case"]{Redo Case Analysis}

For the @tech{empty interface}, we simply add a case to the private,
recursive macro:

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-splicing-syntax-class cond-clause
    #:attributes ()
    (pattern [condition:expr result:expr])
    (pattern [condition:expr #:apply get-result:expr])
    (pattern (~seq #:do body:expr))))
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_ c:cond-clause ...)
     #'(my-cond* c ...)]))
(code:comment "(my-cond* CondClause ...) : Expr")
(define-syntax my-cond*
  (syntax-parser
    [(_)
     #'(void)]
    [(_ [condition:expr result:expr] more ...)
     #'(if condition
           result
           (my-cond* more ...))]
    [(_ [condition:expr #:apply get-result:expr] more ...)
     #'(let ([condition-value condition])
         (if condition-value
             (get-result condition-value)
             (my-cond* more ...)))]
    [(_ #:do body:expr more ...)
     #'(let ()
         body
         (my-cond* more ...))]))
]


@; ----------------------------------------
@subsection[#:tag "multi-codegen"]{Code Generator}

With the @tech{code generator} strategy, the new implementation simply involves
two changes to the old implementation. We must change
@racket[define-syntax-class] to @racket[define-splicing-syntax-class], and we
must add the third variant as below. The definition of @racket[my-clause] itself
does not change.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (define-splicing-syntax-class cond-clause
    #:attributes (make-code) (code:comment "Syntax[Expr] -> Syntax[Expr]")
    (pattern [condition:expr result:expr]
             #:attr make-code (lambda (fail-expr)
                                #`(if condition result #,fail-expr)))
    (pattern [condition:expr #:apply get-result:expr]
             #:attr make-code (lambda (fail-expr)
                                #`(let ([condition-value condition])
                                    (if condition-value
                                        (get-result condition-value)
                                        #,fail-expr))))
    (pattern (~seq #:do body:expr)
             #:attr make-code (lambda (fail-expr)
                                #`(let ()
                                    body
                                    #,fail-expr)))))
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_ c:cond-clause ...)
     (foldr (lambda (make-code rec-expr)
              (make-code rec-expr))
            #'(void)
            (datum (c.make-code ...)))]))
]

@; ----------------------------------------
@subsection[#:tag "multi-ast"]{AST}

@exercise[#:tag "multi-cond/ast"]{Adapt the solution from @secref["enum-ast"] to
support @racket[#:do]-clauses.}


@; ------------------------------------------------------------
@section[#:tag "optional-shapes"]{Optional Shapes}

A common kind of multi-term shape is one that has two (or more variants), one of
which consists of zero terms. A good naming convention for such shapes and
syntax classes is to start them with ``maybe'' or ``optional''. For example, we
could add an optional final @racket[#:else] clause to @racket[my-cond], like this:
@codeblock{
;; (my-cond CondClause ... MaybeFinalCondClause) : Expr
}
where @shape{MaybeFinalCondClause} is defined as follows:
@codeblock{
;; MaybeFinalCondClause ::= ε | #:else Expr
}
Here I've used @shape{ε} to represent zero terms.

The corresponding syntax class for @shape{MaybeFinalCondClause} must be a
splicing syntax class. The interpretation of the possible final clause is that
it provides a condition-free result if none of the previous clauses were
selected; if absent, the result is @racket[(void)]. So we can represent the
interpretation with a single attribute holding an expression:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-splicing-syntax-class maybe-final-cond-clause
    #:attributes (result) (code:comment "Expr")
    (pattern (~seq)
             #:with result #'(void))
    (pattern (~seq #:else result:expr))))
]

Here is the macro, starting from the code-generation implementation above. The
only changes are to the pattern and the use of @racket[#'fc.result] instead of
@racket[#'(void)] in the call to @racket[foldr].

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(define-syntax my-cond
  (syntax-parser
    [(_ c:cond-clause ... fc:maybe-final-cond-clause)
     (foldr (lambda (make-code rec-expr)
              (make-code rec-expr))
            #'fc.result
            (datum (c.make-code ...)))]))
]

@;{
To avoid ambiguity, we must check that @shape{CondClause} and
@shape{MaybeFinalCondClause} don't overlap. They don't.
}


@; ------------------------------------------------------------
@section[#:tag "shapes-types-scopes"]{Shapes, Types, and Scopes (@STAR)}

In @secref["basic-id"] and @secref["basic-type"] we explored how to express
scoping and type relationships between parts of a shape. Can we extend the
notation to express the scoping of the @racket[#:do] form of @shape{CondClause}?

Recall the example program:
@racketblock[
(define ls '((a 1) (b 2) (c 3)))  (code:comment "(Listof (list Symbol Integer))")
(define entry (assoc 'b ls))      (code:comment "(list Symbol Integer)")
(my-cond [(not entry) (error "not found")]
         #:do (define value (cadr entry))
         [(even? value) 'even]
         [(odd? value) 'odd])
]

How does each clause affect the environment of subsequent clauses? The first
clause has no effect on the environments of the following clauses. The second
clause adds a variable binding @racket[value] with type @type{Integer}. More
generally, since we could define multiple variables using @racket[define-values]
or combine definitions with @racket[begin], each clause might bind a @emph{set}
of variables, and each variable has a corresponding type. The first clause
produces no bindings (so @type{∅}, the empty set); the second set produces
@type{{value:Integer}}; the third and fourth clauses also produce
@type{∅}. Let's add a parameter to @shape{CondClause} representing the bindings
it ``produces'' --- that is, the bindings it adds to the environments of
subsequent clauses. We need to change the way we write the shape definition:
@codeblock{
;; CondClause[∅] ::= [Expr Expr]
;; CondClause[∅] ::= [Expr #:apply Expr]
;; CondClause[Δ] ::= #:do Body[Δ]
}
We need the same information from the @shape{Body} shape. Note that @shape{Δ}
does not stand for a type; it stands for a set of pairs of names and types ---
that is, a fragment of a type environment.

In the second clause of this example, @shape{Δ} is @shape{{value:Integer}}. That
is:
@racketblock[
#:do (define value (cadr entry))   @#,shape{: CondClause[{value:Integer}]}
]
because
@racketblock[
(define value (cadr entry))        @#,shape{: Body[{value:Integer}]}
]

We also need to change the way we talk about lists of clauses. The notation
@shape{CondClause ...} doesn't give us a good way to talk about the relationship
between different clauses. Instead, let's define a multi-term shape called
@shape{CondClauses}:

@codeblock{
;; CondClauses ::= ε
;; CondClauses ::= CondClause[Δ] CondClauses{Δ}
}

By @shape{CondClauses{Δ}} I mean that all expressions, body terms, etc within
the clause are in the scope of the additional bindings described by
@shape{Δ}. That is, an environment annotation is implicitly propagated to all of
a shape's sub-shapes. The second line says that in @shape{CondClauses} sequence,
if one clause produces some bindings, then subsequent clauses are in their scope.

Here are the shape definitions with the environment annotations made fully
explicit, where @shape{Γ} stands for a type environment:

@codeblock{
;; CondClause{Γ}[∅] ::= [Expr{Γ} Expr{Γ}]
;; CondClause{Γ}[∅] ::= [Expr{Γ} #:apply Expr{Γ}]
;; CondClause{Γ}[Δ] ::= #:do Body{Γ}[Δ]

;; CondClauses{Γ} ::= ε
;; CondClauses{Γ} ::= CondClause[Δ] CondClauses{Γ,Δ}
}

Finally, here is the shape of @racket[my-cond]:
@codeblock{
;; (my-cond CondClauses)    : Expr      -- implicit
;; (my-cond CondClauses{Γ}) : Expr{Γ}   -- explicit
}

That shows how to use the shape notation to specify type and scoping
relationships between components of shapes.

For many macros, it is probably unnecessary to put this level of detail in the
shape declarations. A more practical approach might be to limit the shapes to
specifying syntactic structure and interpretation, as we've been doing, and
describe the scoping of shapes and macros in prose.

On the other hand, a more precise specification is sometimes useful when writing
macros with complicated binding structures. So keep this tool in mind in case
you need it.
