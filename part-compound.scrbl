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
@title[#:tag "compound-shapes" #:version ""]{Compound Shapes}

@; ------------------------------------------------------------
@section[#:tag "list-shapes"]{List Shapes}

The main kind of @emph{compound shape} is the @emph{list shape}, describing list
terms of fixed or varying length. Actually, we have already been using list
shapes to describe a macro's arguments: a macro transformer function in fact
receives exactly one argument, corresponding to the whole macro use
term. Generally, that is a list term with the macro identifier first and the
arguments making up the rest of the list.

We can add additional levels of grouping to the arguments. For example, here's a
variant of @racket[my-and-let] that groups the identifier with the expression
that provides its value:

@examples[#:eval the-eval #:no-result
(code:comment "(my-and-let2 [x:Id Expr] Expr{x}) : Expr")
(define-syntax my-and-let2
  (syntax-parser
    [(_ [x:id e1:expr] e2:expr)
     #'(let ([x e1])
         (if x e2 #f))]))
]

By itself, though, this change isn't very interesting. The real utility of list
shapes (and patterns, and templates) is in their interaction with enumeration
shapes and ellipses. We'll discuss ellipses now as a special case and discuss
enumeration shapes later.


@; ------------------------------------------------------------
@section[#:tag "ellipses-simple"]{Ellipses with Simple Shapes}

Ellipses mean zero or more repetitions of the preceding shape, pattern, or
template. They are like the star (*) operator in regular expressions. For
example, here is the shape of Racket's @racket[and] macro:

@codeblock{
;; (and Expr ...) : Expr
}

How can we implement our own macro with this shape? One strategy is to use
recursion and case analysis:

@examples[#:eval the-eval #:no-result
(code:comment "(my-and Expr ...) : Expr")
(define-syntax my-and
  (syntax-parser
    [(_)
     #'#t]
    [(_ e1:expr e:expr ...)
     #'(if e1 (my-and e ...) #f)]))
]

(This isn't quite like Racket's @racket[and], which returns the value of the
last expression if all previous expressions were true, and it evaluates the last
expression in tail position. But it's close enough to illustrate ellipses and
recursive macros.)

This macro divides one shape into two patterns: zero expressions or at least one
expression. If we use @racket[my-and] as follows:
@racketblock[
(my-and (odd? 1) (even? 2) (odd? 3))
]
then the first pattern fails to match, but the second pattern matches with
@racket[e1] = @racket[(odd? 1)] and @racket[e ...] = @racket[(even? 2) (odd?
3)]. Note that @racket[e] doesn't match a single term; it matches a sequence of
terms, and when we use @racket[e] in the template, we must follow it with
ellipses. One expansion step rewrites this program to the following:

@racketblock[
==> (if (odd? 1) (my-and (even? 2) (odd? 3)) #f)
]

Where once there were three, now there are only two expressions in the remaining
call to @racket[my-and]. Subsequent steps rewrite that to one, and then none,
and then @racket[my-and]'s base case matches and it disappears entirely:

@racketblock[
==> (if (odd? 1) (if (even? 2) (my-and (odd? 3)) #f) #f)
==> (if (odd? 1) (if (even? 2) (if (odd? 3) (my-and) #f) #f) #f)
==> (if (odd? 1) (if (even? 2) (if (odd? 3) #t #f) #f) #f)
]

Must we always use recursion and case analysis to implement macros with ellipsis
shapes? No; sometimes we can rewrite it into another variable-arity form or
function. For example, here is another definition of @racket[my-and] that relies
on Racket's @racket[andmap] function, ``thunking'' to delay evaluation, and the
variable-arity @racket[list] function:

@examples[#:eval the-eval #:no-result
(code:comment "(my-and Expr ...) : Expr")
(define-syntax my-and
  (syntax-parser
    [(_ e:expr ...)
     #'(andmap (lambda (thunk) (thunk))
               (list (lambda () e) ...))]))
]

For a common, simple macro like @racket[and], this is (likely) not a good
implementation because of run-time overhead, but for other macros this kind of
implementation might be reasonable.

Here is another implementation of @racket[my-and], where the macro itself is not
recursive but the transformer uses a recursive compile-time helper function:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (code:comment "my-and-helper : (Listof Syntax[Expr]) -> Syntax[Expr]")
  (define (my-and-helper exprs)
    (if (pair? exprs)
        #`(if #,(car exprs)
              #,(my-and-helper (cdr exprs))
              #f)
        #'#t)))
(code:comment "(my-and Expr ...) : Expr")
(define-syntax my-and
  (syntax-parser
    [(_ e:expr ...)
     (my-and-helper (syntax->list #'(e ...)))]))
]

The compile-time @racket[my-and-helper] function takes a list of syntax objects
representing expressions and combines them into a single syntax object
representing an expression. Whenever possible, annotate the @type{Syntax} type
with the shape of the term that the syntax object represents: for example,
@type{Syntax[Expr]}, @type{Syntax[(Expr ...)]}, etc. The function uses
@racket[quasisyntax] (which has the reader abbreviation @litchar{#`}) to create
syntax from a template that allows @racket[unsyntax] escapes (@litchar{#,}) to
compute sub-terms. The macro calls the helper with a list of syntax
objects. First it uses ellipses to form the syntax list containing all of the
argument expressions: @racket[#'(e ...)]. This value has the type
@type{Syntax[(Expr ...)]}. Then it calls @racket[syntax->list], which unwraps
the syntax list into a list of syntax --- in this case, specifically, a
@type{(Listof Syntax[Expr])}.

Because it is defined in the transformer environment (or ``at phase 1''), you
cannot directly call @racket[my-and-helper] at the REPL to explore its
behavior. But you can call it using @racket[phase1-eval] special form. Keep in
mind that the whole argument to @racket[phase1-eval] is a compile-time
expression, so you cannot refer to any run-time variables. Also,
@racket[phase1-eval] must be told how to convert the phase-1 (compile-time)
answer into an expression to produce a phase-0 (run-time) value. When the result
type is syntax, use @racket[quote-syntax] if you want to preserve the
syntax-nature of the result; otherwise, use @racket[quote]. For example:

@examples[#:eval the-eval #:label #f
(phase1-eval (my-and-helper (list #'(odd? 1) #'(even? 2)))
             #:quote quote-syntax)
(phase1-eval (my-and-helper (list #'(odd? 1) #'(even? 2)))
             #:quote quote)
]

Here is yet another variation that avoids defining a separate compile-time
helper function and just uses Racket's @racket[foldr]:

The compile-time helper function could also be written as follows:
@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(my-and Expr ...) : Expr")
(define-syntax my-and-helper
  (syntax-parser
    [(_ e:expr ...)
     (foldr (lambda (expr_ rec-code)
              #`(if #,expr_ #,rec-code #f))
            #'#t
            (syntax->list #'(e ...)))]))
]

We can still use @racket[phase1-eval] to explore more complicated compile-time
expressions:
@examples[#:eval the-eval #:label #f #:escape UNQUOTE
(phase1-eval (foldr (lambda (expr_ rec-code)
                      #`(if #,expr_ #,rec-code #f))
                    #'#t
                    (syntax->list #'((odd? 1) (even? 2)))))
]

@; ------------------------------------------------------------
@section[#:tag "ellipses-compound"]{Ellipses with Compound Shapes}

Ellipses can also be used with compound shapes.For example, here is the shape of
a simplified version of @racket[cond] (it doesn't support @racket[=>] and
@racket[else] clauses):

@codeblock{
;; (my-cond [Expr Expr] ...) : Expr
}

Here's a recursive implementation:

@examples[#:eval the-eval #:no-result
(code:comment "(my-cond [Expr Expr] ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ [condition1:expr result1:expr] more ...)
     #'(if condition1
           result1
           (my-cond more ...))]))
]

Here is a non-recursive implementation of @racket[my-cond]. It relies on
Racket's variadic @racket[or] form and the fact that @racket[and] and
@racket[or] treat any value other than @racket[#f] as true, and return that
value to represent a true result.

@examples[#:eval the-eval #:no-result
(code:comment "(my-cond [Expr Expr] ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_ [condition:expr result:expr] ...)
     #'((or (and condition (lambda () result))
            ...
            void))]))
]

Here is another, using a recursive run-time helper function:
@examples[#:eval the-eval #:no-result
(code:comment "(my-cond [Expr Expr] ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_ [condition:expr result:expr] ...)
     #'(my-cond-helper (list (lambda () condition) ...)
                       (list (lambda () result) ...))]))
(code:comment "my-cond-helper : (Listof (-> Any)) (Listof (-> X)) -> X")
(code:comment "PRE: condition-thunks and result-thunks have the same length")
(define (my-cond-helper condition-thunks result-thunks)
  (if (pair? condition-thunks)
      (if ((car condition-thunks))
          ((car result-thunks))
          (my-cond-helper (cdr condition-thunks)
                          (cdr result-thunks)))
      (void)))
]

Lesson: Many macros can be decomposed into two parts: a compile-time part that
adds @racket[lambda] wrappers to handle scoping and delayed evaluation, and a
run-time part that implements the computation and behavior of the macro.

@;{
Exercise: define my-cond using a compile-time helper function
}


@; ------------------------------------------------------------
@section[#:tag "shape-defs"]{User-Defined Shapes}

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

We can also define a syntax class corresponding to the new shape:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "CondClause ::= [Expr Expr]   -- represent condition, result")
  (define-syntax-class cond-clause
    #:attributes (condition result)
    (pattern (condition:expr result:expr))))
]
My convention is to use capitalized names such as @shape{Expr}, @shape{Id}, and
@shape{CondClause} for shapes and lower-case names such as @racket[expr],
@racket[id], and @racket[cond-clause] for syntax classes. Distinguishing them
serves as a reminder that syntax classes represent some but not all of the
meaning of shapes, just like Racket's contracts capture some but not all of the
meaning of types. The syntax class checks that terms have the right structure,
and the attribute names hint at their intended interpretation, but the syntax
class cannot enforce that interpretation.

We update the macro's shape, and we update the implementation's pattern to use a
pattern variable annotated with the new syntax class
(@racket[c:cond-clause]). In the template, we refer to the pattern variable's
@emph{nested attributes} defined by the syntax class (@racket[c.condition] and
@racket[c.result]).

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

It can lead to earlier detection of syntax errors and better error messages,
because the error is reported in terms of the original expression the user
wrote, as opposed to one created by the macro for recursion. The cost is that
the syntax-class check is performed again and again on later arguments; the
number of @racket[cond-clause] checks performed by this version is quadratic in
the number of clauses it originally receives. There is no single right
trade-off. One alternative is to make the public @racket[my-cond] macro check
all of the clauses and then expand into a private recursive helper macro that
only parses one clause at a time.
