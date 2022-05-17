;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse racket/match syntax/macro-testing
                     racket/port rackunit))

@(define the-eval (make-malr-eval))
@(the-eval '(require))

@; ============================================================
@title[#:tag "defshape" #:version ""]{Shape Definitions}

This section shows how to define new shapes and their corresponding
syntax classes.

This section also introduces a shape for simple expressions.

@; ------------------------------------------------------------
@section[#:tag "defshape-def"]{Defining New Shapes}

Consider the shape we've given to @racket[my-cond]:

@codeblock{
;; (my-cond [Expr Expr] ...) : Expr
}

This tells us the structure of @racket[my-cond]'s arguments, but it gives us no
hook upon which to hang a description of the arguments' interpretation. Let's
give it a name:

@codeblock{
;; CondClause ::= [Expr Expr]   -- represent condition, result
}

Now when we describe the behavior of @racket[my-cond], we can separate out the
structure and interpretation of @shape{CondClause}s from the discussion of
@racket[my-cond] itself.
@itemlist[

@item{The @racket[my-cond] form takes a sequence of @shape{CondClauses}, and
that it tries each @shape{CondClause} in order until one is selected, and the
result of the @racket[my-cond] expression is the result of the selected
@shape{CondClause}, or @racket[(void)] if none was selected.}

@item{A @shape{CondClause} consists of two expressions. The first represents a
condition; if it evaluates to a true value, then the clause is selected. The
second expression determines the clause's result.}

]

I typically write something like the terse comment above in the source code and
include the longer, more precise version in the documentation.

We should also define a syntax class, @racket[cond-clause], corresponding to the
new shape:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes (condition result) (code:comment "Expr, Expr")
    (pattern [condition:expr result:expr])))
]
The syntax class has a single @racket[pattern] form specifying the structure of
the terms it accepts. The pattern variables are exported from the syntax class
as @emph{syntax-valued attributes}. I've written a comment after the
@racket[#:attributes] declaration with the shape of each attribute; they both
contain @shape{Expr} terms.

My convention is to use capitalized names such as @shape{Expr}, @shape{Id}, and
@shape{CondClause} for shapes and lower-case names such as @racket[expr],
@racket[id], and @racket[cond-clause] for syntax classes. Distinguishing them
serves as a reminder that syntax classes represent some but not all of the
meaning of shapes, just like Racket's contracts capture some but not all of the
meaning of types. The syntax class checks that terms have the right structure,
and its attribute names hint at their intended interpretation, but the syntax
class cannot enforce that interpretation.

We update the macro's shape to refer to the new shape name, and we update the
implementation's pattern to use a pattern variable annotated with the new syntax
class (@racket[c:cond-clause]). In the template, we refer to the pattern
variable's @emph{attributes} defined by the syntax class (@racket[c.condition]
and @racket[c.result]).

@(define (test-my-cond1)
   @examples[#:eval the-eval #:hidden
   (assert (equal? (my-cond [(even? 1) 'no] [(odd? 3) 'yes]) 'yes))
   (assert (equal? (my-cond [(even? 2) 'ok] [(/ 0) 'whoops]) 'ok))
   (assert (equal? (my-cond [#f 1] [#f 2]) (void)))
   ])

@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ c:cond-clause more ...)
     #'(if c.condition
           c.result
           (my-cond more ...))]))
]
@(test-my-cond1)

In addition to improved organization, another benefit of defining
@racket[cond-clause] as a syntax class is that @racket[my-cond] now
automatically uses @racket[cond-clause] to help explain syntax errors. For
example:

@examples[#:eval the-eval #:label #f
(eval:error (my-cond 5))
(eval:error (my-cond [#t #:whoops]))
]

In the implementation above, should we also annotate @racket[more] to check that
all of the arguments are clauses, instead of only checking the first clause at
each step? That is:

@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ c:cond-clause more:cond-clause ...)
     #'(if c.condition
           c.result
           (my-cond more ...))]))
]
@(test-my-cond1)

It can lead to earlier detection of syntax errors and better error messages,
because the error is reported in terms of the original expression the user
wrote, as opposed to one created by the macro for recursion. The cost is that
the syntax-class check is performed again and again on later arguments; the
number of @racket[cond-clause] checks performed by this version is quadratic in
the number of clauses it originally receives. One solution is to make the public
@racket[my-cond] macro check all of the clauses and then expand into a private
recursive helper macro that only interprets one clause at a time.

@examples[#:eval the-eval #:no-result
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
    [(_ c:cond-clause more ...)
     #'(if c.condition
           c.result
           (my-cond* more ...))]))
]
@(test-my-cond1)

This tension arises because a syntax class has two purposes: validation and
interpretation. For a single term, validation should precede interpretation, but
if a macro has many arguments (for example, a use of @racket[my-cond] might have
many @shape{CondClause}s), how should we interleave validation and
interpretation of the many terms? One appealing goal is to validate all
arguments before interpreting any of them. Another appealing goal is to only
``call'' a syntax class once per term. Each goal constrains the ways we can
define the syntax class and write the macro; achieving both goals is especially
tricky.

A related question is this: How much of the task of interpeting a term belongs
to the syntax class versus the macro that uses it? The division of
responsibility between syntax class and macro affects the @emph{interface}
between them, and that interface affects how the macro is written. This question
becomes more complicated when we add variants to a syntax class; we discuss the
difficulties and solutions in detail in @secref["enum-shapes"].


@; ------------------------------------------------------------
@section[#:tag "defshape-same-diff"]{Same Structure, Different Interpretation}

Recall @exercise-ref["compound:my-evcase1"]. The goal was to design a macro
@racket[my-evcase1] with the following shape:
@codeblock{
;; (my-evcase1 Expr [Expr Expr] ...) : Expr
}
The exercise's description of the macro's behavior referred to ``clauses'',
which is a hint that we should improve the specification by naming that argument
shape. Let's do that now.

We already have a name for the shape @shape{[Expr Expr]}; should we simply
define the shape of @racket[my-evcase1] in terms of @shape{CondClause}? (Perhaps
we should also generalize the name to @shape{ClauseWith2Exprs} so it doesn't
seem so tied to @racket[my-cond]?)

No. The structure of the two shapes is the same, but the interpretation is
different. Specifically, the first expression of a @shape{CondClause} is treated
as a condition, but the first expression of a @racket[my-evcase1] clause is
treated as a value for equality comparison. Furthermore, the two macros happen
to have the same clause structure now, but if we add features to one or the
other (and we will), they might evolve in different ways. In fact, they are
likely to evolve in different ways @emph{because} they have different
interpretations.

So let's define a new shape, @shape{EC1Clause}, for @racket[my-evcase1] clauses:
@codeblock{
;; EC1Clause ::= [Expr Expr]    -- comparison value, result
}
Here is the corresponding syntax class:
@examples[#:eval the-eval #:hidden
(begin-for-syntax
  (define-syntax-class ec1-clause
    #:attributes (comparison result) (code:comment "Expr, Expr")
    (pattern [comparison:expr result:expr])))
]
Now the macro has the shape
@codeblock{
;; (my-evcase1 Expr EC1Clause ...) : Expr
}

@(define (test-my-evcase1)
   @examples[#:eval the-eval #:hidden
   (define-syntax-rule (result+output e)
     (let ([out (open-output-string)])
       (define result
         (parameterize ((current-output-port out))
           (#%expression e)))
       (list result (get-output-string out))))
   (assert (equal?
            (result+output
             (my-evcase1 (begin (printf "got a coin!\n") (* 5 5))
               [5 "nickel"] [10 "dime"] [25 "quarter"] [(/ 0) "infinite money!"]))
            (list "quarter" "got a coin!\n")))
   ])

One implementation strategy is to use @racket[my-cond] as a helper macro. Here's
a first attempt that isn't quite right:
@examples[#:eval the-eval #:no-result
(define-syntax my-evcase1
  (syntax-parser
    [(_ find:expr c:ec1-clause ...)
     #'(my-cond [(equal? find c.comparison) c.result] ...)]))
]
These examples illustrate the problem:
@examples[#:eval the-eval #:label #f
(my-evcase1 (begin (printf "got a coin!\n") (* 5 5))
  [5 "nickel"] [10 "dime"] [25 "quarter"] [(/ 0) "infinite money!"])
(define coins '(25 5 10 5))
(define (get-coin) (begin0 (car coins) (set! coins (cdr coins))))
(eval:error
(my-evcase1 (get-coin)
  [5 "nickel"] [10 "dime"] [25 "quarter"] [(/ 0) "infinite money!"]))
]
The initial expression is re-evaluated for every comparison, which is
problematic if the expression has side-effects.

Here is a fixed implementation that uses a temporary variable to hold the value
of the first expression:
@examples[#:eval the-eval #:no-result
(define-syntax my-evcase1
  (syntax-parser
    [(_ find:expr c:ec1-clause ...)
     #'(let ([tmp find])
         (my-cond [(equal? tmp c.comparison) c.result] ...))]))
]
@(test-my-evcase1)

Now the examples behave as expected:
@examples[#:eval the-eval #:label #f
(my-evcase1 (begin (printf "got a coin!\n") (* 5 5))
  [5 "nickel"] [10 "dime"] [25 "quarter"] [(/ 0) "infinite money!"])
(define coins '(25 5 10 5))
(define (get-coin) (begin0 (car coins) (set! coins (cdr coins))))
(my-evcase1 (get-coin)
  [5 "nickel"] [10 "dime"] [25 "quarter"] [(/ 0) "infinite money!"])
]

@exercise[#:tag "defshape-test"]{Turn the examples above into test cases for
@racket[my-evcase1]. Check that the tests fail on the original version of the
macro and succeed on the fixed version.

The @racket[catch-output] macro from @exercise-ref["basic:catch-output"] and
@secref["basic-hygiene2"] is not quite enough to express these tests
conveniently. Write a more general helper macro with the following shape:
@codeblock{
;; (result+output Expr[R]) : Expr[(list R String)]
}
and use that to express your tests.}


@; ------------------------------------------------------------
@section[#:tag "simple-expr"]{Helper Macros and Simple Expressions}

Recall the implementation strategies for handling ellipsis shapes from
@secref["ellipses-simple"]. The first strategy was to write a recursive
macro. Is it possible to implement @racket[my-evcase1] using that strategy?

No. It is not possible to implement @racket[my-evcase1] as a recursive macro,
according to the shape we've given it, while guaranteeing that we evaluate the
initial expression once. Compare this with fact that some list functions cannot
be written purely as structural recursive functions. The @racket[average]
function is a good example: it can only be expressed by combining or adjusting
the results of one or more structurally recursive helper functions.

We can, however, implement @racket[my-evcase1] using a recursive helper
macro. In fact, we've done that in the previous implementation, by using
@racket[my-cond]. But let's try a different implementation using a recursive
macro that has a shape that is similar to, although not identical to, that of
@racket[my-evcase1]. In particular, it is worth talking about the shape involved
in the interface between the main, public macro and its private helper.

@examples[#:eval the-eval #:no-result
(code:comment "(my-evcase1 Expr EC1Clause ...) : Expr")
(define-syntax my-evcase1
  (syntax-parser
    [(_ find:expr c:ec1-clause ...)
     #'(let ([tmp find])
         (my-evcase1* tmp c ...))]))
]
So, what is the shape of the helper macro, @racket[my-evcase1*]?

We could describe @racket[my-evcase1*] with the following shape:
@codeblock{
;; (my-evcase1* Expr EC1Clause ...) : Expr
}
but that's the same shape as @racket[my-evcase1], so if we're using the shapes
to guide our design --- specifically, our implementation options --- we have not
made any progress. The point of @racket[my-evcase1*] is that its first argument
is a simple variable reference, not any arbitrary expression whose evaluation
might be costly or involve side effects. Let's reflect that in the shape:
@codeblock{
;; (my-evcase1* SimpleExpr EC1Clause ...) : Expr
}

The @shape{SimpleExpr} shape is like @shape{Expr}, except that it only contains
expressions that we are willing to duplicate. That is, the expansion of the
expression is simple and small, and the evaluation of the expression is trivial
and does not involve side effects. Acceptable expressions include quoted
constants and variable references. Usually, we also expect simple expressions to
be constant, so a variable reference should be to a fresh local variable that is
never mutated. Depending on the situation, there might be other expressions that
we would accept as simple.

There is no separate syntax class for @shape{SimpleExpr}; just use @racket[expr]
or omit the syntax class annotation. It is infeasible to @emph{check} whether an
expression is simple; instead, you should only make private macros accept
@shape{SimpleExpr} arguments, and you should check that all of the public macros
that call them pass appropriate expressions.

In this example, let's assume that @racket[my-evcase1*] is private and only
@racket[my-evcase] calls it. The initial expression that @racket[my-evcase]
gives to the helper is a local variable reference, which is simple.

Here is a recursive implementation of @racket[my-evcase1*]:
@examples[#:eval the-eval #:no-result
(code:comment "(my-evcase1* SimpleExpr EC1Clause ...) : Expr")
(define-syntax my-evcase1*
  (syntax-parser
    [(_ tmp)
     #'(void)]
    [(_ tmp c:ec1-clause more ...)
     #'(if (equal? tmp c.comparison)
           c.result
           (my-evcase1* tmp more ...))]))
]
@(test-my-evcase1)
Note that the second clause duplicates the @racket[tmp] argument.


@(close-eval the-eval)
