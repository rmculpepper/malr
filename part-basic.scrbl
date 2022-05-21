;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse racket/match syntax/macro-testing
                     racket/port racket/block racket/string rackunit))

@(define the-eval (make-malr-eval))
@(the-eval '(require racket/port racket/block racket/string))

@; ============================================================
@title[#:tag "basic-shapes" #:version ""]{Basic Shapes}

This section introduces the most important basic shapes for macro design.


@; ------------------------------------------------------------
@section[#:tag "basic-expr"]{The Expr (Expression) Shape}

The @shape{Expr} shape represents the intention to interpret the term as a
Racket expression by putting it in an expression context. In general, a macro
cannot check a term and decide whether it is a valid expression; only the Racket
macro expander can do that. As a pragmatic approximation, the @shape{Expr} shape
and its associated @racket[expr] syntax class exclude only keyword terms, like
@racket[#:when], so that macros can detect and report misuses of keyword
arguments.

As an example, let's implement @racket[my-when], a simple version of Racket's
@racket[when] form. It takes two expressions; the first is the condition, and
the second is the result to be evaluated only if the condition is true. Here is
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
We use the @racket[expr] syntax class to annotate pattern variables that have
the @shape{Expr} shape. Note that the names of the pattern variables do not
include the @racket[:expr] annotation, so in the syntax template we simply
write @racket[condition] and @racket[result].

To test the macro, we rephrase the previous examples as tests:
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

@; ----------------------------------------
@exercise[#:tag "basic:shape-violation"]{Each of the following uses of
@racket[my-when] violates its declared shape:
@itemlist[
@item{@racket[(my-when #:true "verity")]}
@item{@racket[(my-when 'ok (define ns '(1 2 3)) (length ns))]}
@item{@racket[(my-when (odd? 1) (begin (define one 1) (+ one one)))]}
@item{@racket[(my-when #f (+ #:one #:two))]}
]
Why? Which examples are rejected by the @racket[my-when] macro itself, and
what happens to the other examples? What difference does it make if you remove
the @racket[expr] syntax class annotations from the macro definition?}

@exercise[#:tag "basic:my-unless"]{Design a macro @racket[my-unless] like
@racket[my-when], except that it negates the condition.}

@exercise[#:tag "basic:catch-output"]{Design a macro @racket[catch-output] that
takes a single expression argument. The expression is evaluated, but its result
is ignored; instead, the result of the macro is a string containing all of the
output written by the expression. For example:

@racketblock[(catch-output (for ([i 10]) (printf "~s" i))) (code:comment "expect \"0123456789\"")]}


@; ------------------------------------------------------------
@section[#:tag "basic-body"]{The Body Shape}

The @shape{Body} shape is like @shape{Expr} except that it indicates that the
term will be used in a body context, so definitions are allowed in addition to
expressions.

There is no distinct syntax class for @shape{Body}; just use @racket[expr].

In practice, the @shape{Body} shape is usually used with ellipses; see
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

Given the new shape, the previous implementation would be wrong, since it
does not place its second argument in a body context. Here is an updated
implementation:
@examples[#:eval the-eval #:no-result
(define-syntax my-when
  (syntax-parser
    [(_ condition:expr result-body:expr)
     #'(if condition (block result-body) (void))]))
]
@examples[#:eval the-eval #:hidden
(assert (equal?
         (my-when (odd? 37)
           (begin (define q (quotient 37 2)) q))
         18))
(assert (equal? (my-when #t (define x 1)) (void)))
(assert (equal? (my-when #f (begin (define x 1) (/ 0))) (void)))
]

That is, use @racket[(block HOLE)] to wrap a @shape{Body} so it can be used in a
strict @shape{Expr} position. It is also common to use a @racket[(let () HOLE)]
wrapper, but that does not work for all @shape{Body} terms; it requires that the
@shape{Body} term ends with an expression. The @racket[block] form is more
flexible.

Racket's @racket[#%expression] form is useful in the opposite situation. It has
the following shape:
@codeblock{
;; (#%expression Expr) : Body
}
That is, use @racket[(#%expression HOLE)] to turn a @shape{Body} position into a
strict @shape{Expr} position.

@exercise[#:tag "basic:catch-output-expr"]{Check your solution to
@exercise-ref["basic:catch-output"]; does the macro also accept @shape{Body}
terms like the one above? That is, does the following work?
@racketblock[
(catch-output (begin (define q (quotient n 2)) (printf "q = ~s\n" q)))
]
If so, ``fix it'' (that is, make it more restrictive) using
@racket[#%expression].}


@; ------------------------------------------------------------
@section[#:tag "basic-hygiene2"]{Proper Lexical Scoping, Part 2}

Here is one solution to @exercise-ref["basic:catch-output"] using
@racket[with-output-to-string]:
@examples[#:eval the-eval #:no-result
(code:comment "(catch-output Expr) : Expr")
(define-syntax catch-output
  (syntax-parser
    [(_ e:expr)
     #'(with-output-to-string (lambda () (#%expression e)))]))
]
Racket already provides @racket[with-output-to-string] from the
@racketmodname[racket/port] library, but if it did not, we could define it as
follows:
@examples[#:eval the-eval #:no-result
(code:comment "with-output-to-string : (-> Any) -> String")
(define (with-output-to-string proc)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      (proc))
    (get-output-string out)))
]

Here is another implementation of @racket[catch-output], which essentially
inlines the definition of @racket[with-output-to-string] into the macro
template:
@examples[#:eval the-eval #:no-result
(code:comment "(catch-output Expr) : Expr")
(define-syntax catch-output
  (syntax-parser
    [(_ e:expr)
     #'(let ([out (open-output-string)])
         (parameterize ((current-output-port out))
           (#%expression e))
         (get-output-string out))]))
]

In @secref["intro-lexical"] we saw that we cannot interfere with a macro's
``free variables'' by shadowing them at the macro use site. For example, the
following attempt to capture the macro's reference to @racket[get-output-string]
fails:
@examples[#:eval the-eval #:label #f
(let ([get-output-string (lambda (p) "pwned!")])
  (catch-output (printf "doing just fine, actually")))
]

But what about the other direction? The macro introduces a binding of a variable
named @racket[out]; could this binding capture references to @racket[out] in the
expression given to the macro? Here is an example:

@examples[#:eval the-eval #:label #f
(let ([out "Aisle 24"])
  (catch-output (printf "The exit is located at ~a." out)))
]

The result shows that the macro's @racket[out] binding does not interfere with
the use-site's @racket[out] variable. We say that the @racket[catch-output]
macro is ``hygienic''.

A macro is @deftech{hygienic} if it follows these two lexical scoping principles:
@itemlist[#:style 'ordered

@item{A @emph{use-site binding} does not capture a @emph{definition-site
reference}.}

@item{A @emph{definition-site binding} does not capture a @emph{use-site
reference}.}

]
Racket macros are hygienic by default. In FIXME-REF we will discuss a few
situations when it is useful to break hygiene.


@; ------------------------------------------------------------
@section[#:tag "basic-id"]{The Id (Identifier) Shape}

The @shape{Id} shape contains all identifier terms.

The @shape{Id} shape usually implies that the identifier will be used as the
name for a variable, macro, or other sort of binding. In that case, we say the
identifer is used as a @emph{binder}.

Use the @racket[id] syntax class for pattern variables whose shape is @shape{Id}.

Let's write a macro @racket[my-and-let] that acts like @racket[and] with two
expressions but binds the result of the first expression to the given identifier
before evaluating the second expression. Here is the shape:
@codeblock{
;; (my-and-let Id Expr Expr) : Expr
}
Here are some examples:
@racketblock[
(define ls '((a 1) (b 2) (c 3)))
(my-and-let entry (assoc 'b ls) (cadr entry)) (code:comment "expect 2")
(my-and-let entry (assoc 'z ls) (cadr entry)) (code:comment "expect #f")
]
@(define (test-my-and-let)
   @examples[#:eval the-eval #:hidden
   (define ls '((a 1) (b 2) (c 3)))
   (assert (equal? (my-and-let entry (assoc 'b ls) (cadr entry)) 2))
   (assert (equal? (my-and-let entry (assoc 'z ls) (cadr entry)) #f))
   ])
Here is an implementation:
@examples[#:eval the-eval #:no-result
(define-syntax my-and-let
  (syntax-parser
    [(_ x:id e1:expr e2:expr)
     #'(let ([x e1]) (if x e2 #f))]))
]
@(test-my-and-let)

The main point of @racket[my-and-let], though, is that if the second expression
is evaluated, it is evaluated in an environment where the identifier is bound to
the value of the first expression. Let's put that information in the shape of
@racket[my-and-let]. It requires two changes:
@itemlist[

@item{Label the identifier so we can refer to it later. So instead of
@shape{Id}, we write @shape{x:Id}. The label does not have to be the same as
the name of the pattern variable, but it makes sense to use the same name here.}

@item{Add an @emph{environment annotation} to the second @shape{Expr} indicating
that it's in the scope of a variable whose name is whatever actual identifier
@shape{x} refers to: @shape{Expr{x}}.}

]

Here is the updated shape for @racket[my-and-let]:
@codeblock{
;; (my-and-let x:Id Expr Expr{x}) : Expr
}

We can check the implementation: @racket[e1] does not occur in the scope of
@racket[x], and @racket[e2] does occur in the scope of @racket[x].

Here is another implementation:
@racketblock[
(code:comment "(my-and-let x:Id Expr Expr{x}) : Expr")
(define-syntax my-and-let
  (syntax-parser
    [(_ x:id e1:expr e2:expr)
     #'(let ()
         (define x e1)    (code:comment "BAD")
         (if x e2 #f))]))
]
This implementation is @emph{wrong}, because @racket[e1] occurs in the scope of
@racket[x], but it should not.

Here is another version:
@examples[#:eval the-eval #:no-result
(code:comment "(my-and-let x:Id Expr Expr{x}) : Expr")
(define-syntax my-and-let
  (syntax-parser
    [(_ x:id e1:expr e2:expr)
     #'(let ()
         (define tmp e1)
         (if tmp (let ([x tmp]) e2) #f))]))
]
@(test-my-and-let)

This implementation is good (although more complicated than unnecessary),
because @racket[e1] no longer occurs in the scope of @racket[x]. But what about
@racket[tmp]? Because of hygiene, the definition of @racket[tmp] introduced by
the macro is not visible to @racket[e1]. (To be clear, it would be @emph{wrong}
to write @shape{Expr{tmp}} for the shape of the first expression.)

@exercise[#:tag "basic:if-let"]{Generalize @racket[my-and-let] to
@racket[my-if-let], which takes an extra expression argument which is the
macro's result if the condition is false. The macro should have the following
shape:
@codeblock{;; (my-if-let x:Id Expr Expr{x} Expr) : Expr}
Double-check your solution to make sure it follows the scoping specified by the
shape.}

@; ------------------------------------------------------------
@section[#:tag "basic-type"]{Expressions, Types, and Contracts}

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

@(define (test-my-match-pair)
   @examples[#:eval the-eval #:hidden
   (assert (equal? (my-match-pair (list 1 2 3) n ns (< n (length ns))) #t))
   (assert (equal? (my-match-pair (list 'p "hello world") tag content
                                  (format "<~a>~a</~a>" tag (string-join content " ") tag))
                   "<p>hello world</p>"))
   ])

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
@(test-my-match-pair)
Note that we introduce a @emph{temporary variable} (or @emph{auxiliary
variable}) named @racket[pair-v] to avoid evaluating the @racket[pair]
expression twice.

We could add more information to the shape. The macro expects the first argument
to be a pair, and whatever types of values the pair contains become the types of
the identifiers:
@codeblock{
;; (my-match-pair Expr[(cons T1 T2)] x:Id xs:Id Expr{x:T1,xs:T2}) : Expr
}
I've written @shape{Expr[(cons T1 T2)]} for the shape of expressions of type
@type{(cons T1 T2)}, where the type @type{(cons T1 T2)} is the type of all pairs
(values made with the @racket[cons] constructor) whose first component has type
@type{T1} and whose second component has type @type{T2}. The second expression's
environment annotation includes the types of the variables. This macro shape is
polymorphic; there is an implicit @shape{forall (T1, T2)} at the beginning of
the declaration.

The result of the macro is the result of the second expression, so the type of
the macro is the same as the type of the second expression. We could add that
to the shape too:
@codeblock{
;; (my-match-pair Expr[(cons T1 T2)] x:Id xs:Id Expr{x:T1,xs:T2}[R]) : Expr[R]
}
Now the second @shape{Expr} has both a environment annotation and a type
annotation. 

When I say ``type'' here, I'm not talking about Typed Racket or some other typed
language implemented in Racket, nor do I mean that there's a super-secret type
checker hidden somewhere in Racket next to a flight simulator. By ``type'' I
mean a semi-formal, unchecked description of expressions and macros that
manipulate them. In this case, the shape declaration for @racket[my-match-pair]
warns the user that the first argument must produce a pair. If it doesn't, the
user has failed their obligations, and the macro may do bad things.

Of course, given human limitations, we would prefer the macro not to do bad
things. Ideally, the macro definition and macro uses could be statically checked
for compliance with shape declarations, but Racket does not not implement such a
checker for macros. (It's complicated.) At least, though, the macro enforce
approximations of the types of expression arguments using @emph{contracts}.

Use the @racket[expr/c] syntax class for a pattern variable whose shape is
@shape{Expr[Type]} when @type{Type} has a useful contract approximation. In this
example, the type @type{(cons T1 T2)} has a useful contract approximation
@racket[pair?], but there is no useful contract for the type @type{R}. The
@racket[expr/c] syntax class takes an argument, so you cannot use the
@litchar{:} notation; you must use @racket[~var] or @racket[#:declare]
instead. The argument is a syntax object representing the contract to apply to
the expression. (It is @racket[#'pair?] instead of @racket[pair?] because the
contract check is performed at run time.) In the syntax template, use the
@racket[c] ("contracted") @emph{attribute} of the pattern variable to get the
expression with a contract-checking wrapper. Here's the contract-checked version
of the macro:

@examples[#:eval the-eval #:no-result
(code:comment "(my-match-pair Expr[(cons T1 T2)] x:Id xs:Id Expr{x:T1,xs:T2}[R]) : Expr[R]")
(define-syntax my-match-pair
  (syntax-parser
    [(_ (~var pair (expr/c #'pair?)) x:id xs:id result:expr)
     #'(let ([pair-v pair.c])   (code:comment "Important: pair.c, not pair")
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
     #'(let ([pair-v pair.c])   (code:comment "Important: pair.c, not pair")
         (let ([x (car pair-v)]
               [xs (cdr pair-v)])
           result))]))
]
@(test-my-match-pair)

Now calling @racket[my-match-pair] raises a contract violation if the first
expression does not produce a pair. For example:
@examples[#:eval the-eval #:label #f
(eval:error (my-match-pair 'not-a-pair n ns (void)))
]

@exercise[#:tag "basic:my-when-contract"]{Modify the @racket[my-when] macro to
check that the condition expression produces a boolean value. (Note: this is not
idiomatic for Racket conditional macros).}

@; ------------------------------------------------------------
@section[#:tag "basic-expr-uses"]{Uses of Expressions}

In general, what can a macro do with an expression (@shape{Expr})?
@itemlist[

@item{It can use the value (or values) that the expression evaluates to. For
example, the behavior of the @racket[my-when] macro depends on the value that
its first expression produces.}

@item{It can determine whether the expression is evaluated or when the
expression is evaluated. The @racket[my-when] example determines whether to
evaluate its second expression. The standard @racket[delay] macro is a classic
example of controlling when an expression is evaluated.}

@item{It can change what dynamic context the expression is evaluated within. For
example, a macro could use @racket[parameterize] to evaluate the expression in a
context with different values for some parameters.}

@item{It can change the static context the expression is evaluated
within. Mainly, this means putting the expression in the scope of additional
bindings, as we did in @racket[my-and-let] and @racket[my-match-pair].}

]

There are some restrictions on what macros can do and should do with expressions:

@itemlist[

@item{@bold{A macro cannot get the value of the expression at compile time.} The
expression represents computation that will occur later, at run time, perhaps on
different machines, perhaps many times with different values in the run-time
environment. A macro can only interact with an expression's value by producing
code to process the value at run time.}

@item{@bold{A macro must not look at the contents of the expression itself.}
Expressions are macro-extensible, so there is no grammar to guide case
analysis. Interpreting expressions is the macro expander's business, so don't
try it yourself. The macro expander is complicated, and if you attempt to
duplicate its work ``just a little'', you are likely to make unjustified
assumptions and get it wrong. For example, an expression consisting of a
self-quoting datum is not necessarily a constant, or even free of side effects;
it might have a nonstandard @racket[#%datum] binding, which could give it any
behavior at all. Likewise, a plain identifier is not necessarily a variable
reference; it might be an identifier macro, or it might have a nonstandard
@racket[#%top] binding.

In later sections (FIXME-REF), we'll talk about how to cooperate with
the macro expander to do case analysis of expressions and other forms.}

@item{In general, a macro should not duplicate an argument expression. That is,
the expression should occur exactly once in the macro's expansion. Duplicating
expressions leads to expanding the same code multiple times, which can lead to
slow compilation and bloated compiled code. The increases to both time and code
size are potentially exponential, if duplicated expressions themselves contain
macros that duplicate expressions and so on.

If you need to refer to an expression's value multiple times, bind it to a
temporary variable. If you need to evaluate the same expression multiple times,
then bind a temporary variable to a thunk containing the expression and then
apply the thunk multiple times.

One exception to this rule is if the macro knows that the expression is
``simple'', like a variable reference or quoted constant, because the macro is
private and all of its use sites can be inspected. We'll discuss this case in
@secref["simple-expr"].}

@item{In general, a macro should evaluate expressions in the same order that
they appear (that is, ``left to right''), unless it has a reason to do
otherwise.

In Racket information generally flows from left to right, and the interpretation
of later terms can depend on earlier terms. For example, @racket[my-when] uses
the value of its first (that is, left-most) expression argument to decide
whether to evaluate its second (that is, right-most) expression. It would be
non-idiomatic syntax design to put the condition expression second and the
result expression first.

Similarly, the scope of an identifier is generally somewhere to the right of the
identifier itself. For example, in @racket[my-match-pair], the identifiers are
in scope in the following expression. If we swapped @racket[my-match-pair]'s
expressions, so it had the shape @shape{(my-match-pair Expr{x,xs} x:Id xs:Id
Expr)}, that would not be idiomatic.}

]

The same principles apply to @shape{Body} terms as well.

@(close-eval the-eval)
