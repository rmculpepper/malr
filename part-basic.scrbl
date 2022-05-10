;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse racket/match syntax/macro-testing
                     racket/port rackunit))

@(define the-eval (make-base-eval))
@(the-eval '(require (only-in racket/base [quote Quote] [syntax Syntax])
                     rackunit syntax/macro-testing racket/port
                     (for-syntax racket/base racket/syntax syntax/parse
                                 (only-in racket/base [quote Quote] [syntax Syntax]))))

@; ============================================================
@title[#:tag "basic-shapes" #:version ""]{Basic Shapes}

@; ------------------------------------------------------------
@section[#:tag "shape:expr"]{The Expr (Expression) Shape}

The @shape{Expr} shape contains all terms except for keywords (like
@racket[#:when]). In principle, there are ways to make keywords legal Racket
expressions (by shadowing Racket's @racket[#%datum] binding), but pragmatically
it is useful to consider expressions and keywords disjoint, so that macros can
detect and report misuses of keyword arguments.

The @shape{Expr} shape represents the intention to interpret the term as a
Racket expression by putting it in a Racket expression context.

For example, let's implement @racket[my-when], a simple version of Racket's
@racket[when] form. It takes two expressions; the first is the condition, and
the second is the result to be evaluated only if the condition is true. Here's
the shape:
@codeblock{
;; (my-when Expr Expr) : Expr
}
Here are some examples:
@racketblock[
(my-when (odd? 5) (printf "odd!\n"))    (code:comment "expect print")
(my-when (even? 5) (printf "even!\n"))  (code:comment "expect no print")
]
Here's the implementation:
@examples[#:eval the-eval #:no-result
(define-syntax my-when
  (syntax-parser
    [(_ condition:expr result:expr)
     #'(if condition result (void))]))
]
Here we use the @racket[expr] syntax class to annotate pattern variables that
have the @shape{Expr} shape. Note that the name of the pattern variable is just
@racket[condition], not @racket[condition:expr], so that's what appears in the
template.

The @racket[expr] annotations cause the macro to automatically report a good
error for misuses of @racket[my-when] like this:
@examples[#:eval the-eval #:label #f
(eval:error (my-when #:truth "verity"))
]
Try removing the annotations and see what the macro does on the bad example.

Here are the previous examples rephrased as tests:
@examples[#:eval the-eval #:label #f
(check-equal? (with-output-to-string
                (lambda () (my-when (odd? 5) (printf "odd!\n"))))
              "odd!\n")
(check-equal? (with-output-to-string
                (lambda () (my-when (even? 5) (printf "even!\n"))))
              "")
(check-exn exn:fail:syntax?
           (lambda ()
             (convert-syntax-error
              (my-when #:truth "verity"))))
]


@; ------------------------------------------------------------
@section[#:tag "shape:body"]{The Body Shape}

The @shape{Body} shape is like @shape{Expr} --- it contains all terms except
keywords --- except that it indicates that the term will be used in a body
context, so definitions are allowed in addition to expressions.

There is no distinct syntax class for @shape{Body}; just use @racket[expr].

In practice, the @shape{Body} shape is used with ellipses; see
@secref["compound-shapes"]. But we can make a version of @racket[my-when] that
takes a single @shape{Body} term, even though it isn't idiomatic Racket
syntax. Here is the shape:

@codeblock{
;; (my-when Expr Body) : Expr
}

Here is an example allowed by the new shape but not by the previous shape:
@racketblock[
(define n 37)
(my-when (odd? n)
  (begin (define q (quotient n 2)) (printf "q = ~s\n" q)))
]

Given the new shape, the previous implementation would be @emph{wrong}, since it
does not place its second argument in a body context. Here is an updated
implementation:
@examples[#:eval the-eval #:no-result
(define-syntax my-when
  (syntax-parser
    [(_ condition:expr result-body:expr)
     #'(if condition (let () result-body) (void))]))
]


@; ------------------------------------------------------------
@section[#:tag "basic-expr"]{The Id (Identifier) Shape}

The @shape{Id} shape contains all identifiers.

The @shape{Id} shape usually implies that the identifier will be used as the
binder for a variable, macro, or other sort of name.

Use the @racket[id] syntax class for pattern variables whose shape is @shape{Id}.

Let's write a macro @racket[my-and-let] that acts like @racket[and] with two
expressions but binds the result of the first expression to the given identifier
before evaluating the second expression. Here is the shape:

@codeblock{
;; (my-and-let Id Expr Expr) : Expr
}
For example:
@racketblock[
(define ls '((a 1) (b 2) (c 3)))
(my-and-let (assoc 'b ls) entry (cadr entry)) (code:comment "expect 2")
(my-and-let (assoc 'z ls) entry (cadr entry)) (code:comment "expect #f")
]
Here is an implementation:
@examples[#:eval the-eval #:no-result
(define-syntax my-and-let
  (syntax-parser
    [(_ x:id e1:expr e2:expr)
     #'(let ([x e1]) (if x e2 #f))]))
]

The main point of @racket[my-and-let], though, is that if the second expression
is evaluated, it is evaluated in an environment where the identifier is bound to
the value of the first expression. Let's put that information in the shape of
@racket[my-and-let]. It requires two changes:
@itemlist[

@item{Name the identifier so we can refer to it later. So instead of
@shape{Id}, we write @shape{x:Id}. The name does not have to be the same as
the name of the pattern variable, but it makes sense to use the same name here.}

@item{Add an @emph{environment annotation} to the second @shape{Expr} indicating
that it's in the scope of a variable whose name is whatever identifier
@racket[x] is bound to: @shape{Expr{x}}.}

]

Here's the updated shape for @racket[my-and-let]:
@codeblock{
;; (my-and-let x:Id Expr Expr{x}) : Expr
}

We can check the implementation: @racket[e1] does not occur in the scope of
@racket[x], and @racket[e2] does occur in the scope of @racket[x]. Consider the
following definition:

@racketblock[
(code:comment "(my-and-let x:Id Expr Expr{x}) : Expr")
(define-syntax my-and-let
  (syntax-parser
    [(_ x:id e1:expr e2:expr)
     #'(let ()
         (define x e1)    (code:comment "BAD")
         (if x e2 #f))]))
]

This implementation is @emph{bad} because @racket[e1] occurs in the scope of
@racket[x], even though it isn't supposed to. Here's another version:

@examples[#:eval the-eval #:no-result
;; (my-and-let x:Id Expr Expr{x}) : Expr
(define-syntax my-and-let
  (syntax-parser
    [(_ x:id e1:expr e2:expr)
     #'(let ()
         (define tmp e1)
         (if x (let ([x tmp]) e2) #f))]))
]

This implementation is good (although more complicated than unnecessary),
because @racket[e1] no longer occurs in the scope of @racket[x]. But what about
@racket[tmp]? Because of hygiene, the definition of @racket[tmp] introduced by
the macro is not visible to @racket[e1]. (To be clear, it would be @emph{wrong}
to write @shape{Expr{tmp}} for the shape of the first expression.)

@bold{FIXME: explain this half of hygiene!}


@; ------------------------------------------------------------
@section[#:tag "expr-type"]{Expressions, Types, and Contracts}

Let's design the macro @racket[my-match-pair], which takes an expression to
destructure, two identifiers to bind as variables, and a result expression. Here
are some examples:
@racketblock[
(my-match-pair (list 1 2 3) n ns (< n (length ns)))
(code:comment "expect #t")
(my-match-pair (list 'p "hello world") tag content
               (format "<~a>~a</~a>" tag (string-join content " ") tag))
(code:comment "expect \"<p>hello world</p>\"")
]

Here is one shape we could write for @racket[my-match-pair]:
@codeblock{
;; (my-match-pair Expr x:Id xs:Id Expr{x,xs}) : Expr
}

Here's an implementation:
@examples[#:eval the-eval #:no-result
(define-syntax my-match-pair
  (syntax-parser
    [(_ pair:expr x:id xs:id result:expr)
     #'(let ([pair-v pair])
         (let ([x (car pair-v)]
               [xs (cdr pair-v)])
           result))]))
]
Note that we introduce a @racket[pair-v] variable to avoid evaluating the
@racket[pair] expression twice.

We could add more information to the shape. The macro expects the first argument
to be a pair, and whatever types of values the pair contains become the types of
the identifiers. Also, the result of the macro is the result of the second
expression, so the type of the macro is the same as the type of the second
expression. Here is the more detailed shape:
@codeblock{
;; (my-match-pair Expr[(cons T1 T2)] x:Id xs:Id Expr{x:T1,xs:T2}[R]) : Expr[R]
}
I've written @shape{Expr[(cons T1 T2)]} for the shape of expressions of type
@type{(cons T1 T2)}, where the type @type{(cons T1 T2)} is the type of all pairs
(values made with the @racket[cons] constructor) whose first component has type
@type{T1} and whose second component has type @type{T2}. The second @shape{Expr}
has both a @emph{environment annotation} and a @emph{type annotation}; unlike
the previous examples with environment annotations, I've included the types of
the variables. This is a polymorphic shape; you can imagine an implicit
@shape{forall (T1, T2, R)} at the beginning of the declaration.

Note: When I say ``type'' here, I'm not talking about Typed Racket or Hackett or
some other typed language implemented in Racket. Nor do I mean that there's a
super-secret type checker hidden in Racket on the top shelf next to the flight
simulator. I'm using ``type'' to mean a semi-formal, unchecked description of
the behavior of expressions and macros that manipulate them. In this case, the
shape declaration for @racket[my-match-pair] warns the user that the first
argument must produce a pair. If it doesn't, the user has failed their
obligations, and the macro may do bad things.

Of course, given human limitations, it is preferable for the macro not to do bad
things. Ideally, the macro definition and macro uses could be statically checked
for compliance with shape declarations, but Racket does not not implement such a
checker for macros. (It's complicated.) At least, though, the macro could use
@emph{contracts} to enforce approximations of the types of expression arguments.

Use the @racket[expr/c] syntax class for a pattern variable whose shape is
@shape{Expr[Type]} when @type{Type} has a useful contract approximation. In this
example, the type @type{(cons T1 T2)} has a useful contract approximation
@racket[pair?], but there is no useful contract for the type @type{R}. The
@racket[expr/c] syntax class takes a parameter, so you cannot use the
@litchar{:} notation; you must use @racket[~var] or @racket[#:declare]
instead. The parameter is a syntax object representing the contract to apply to
the expression. (It is @racket[#'pair?] instead of @racket[pair?] because the
contract check is performed at run time.) Finally, use the @racket[c]
("contracted") @emph{attribute} of the pattern variable to get the
contract-wrapped version of the expression. Here's the contract-checked version
of the macro:

@examples[#:eval the-eval #:no-result
(code:comment "(my-match-pair Expr[(cons T1 T2)] x:Id xs:Id Expr{x:T1,xs:T2}[R]) : Expr[R]")
(define-syntax my-match-pair
  (syntax-parser
    [(_ (~var pair (expr/c #'pair?)) x:id xs:id result:expr)
     #'(let ([pair-v pair.c])
         (let ([x (car pair-v)]
               [xs (cdr pair-v)])
           result))]))
]
Here's the implementation using @racket[#:declare] instead of @racket[~var]:
@examples[#:eval the-eval #:no-result
;; (my-match-pair Expr[(cons T1 T2)] x:Id xs:Id Expr{x:T1,xs:T2}[R]) : Expr[R]
(define-syntax my-match-pair
  (syntax-parser
    [(_ pair x:id xs:id result:expr)
     #:declare pair (expr/c #'pair?)
     #'(let ([pair-v pair.c])
         (let ([x (car pair-v)]
               [xs (cdr pair-v)])
           result))]))
]

Now calling @racket[my-match-pair] raises a contract violation if the first
expression does not produce a pair:
@examples[#:eval the-eval #:label #f
(eval:error (my-match-pair 'not-a-pair n ns (void)))
]
