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
@title[#:tag "compound-shapes" #:version ""]{Compound Shapes}

This section introduces compound shapes, including list shapes and ellipsis
shapes. It discusses several implementation strategies for macros consuming
ellipsis shapes.


@; ------------------------------------------------------------
@section[#:tag "list-shapes"]{List Shapes}

The main kind of @emph{compound shape} is the @emph{list shape}, describing list
terms of fixed or varying length. Actually, we have already been using list
shapes to describe a macro's arguments: a macro transformer function in fact
receives exactly one argument, corresponding to the whole macro-use
term. Typically, that is a list term with the macro identifier first and the
arguments making up the rest of the list.

@; FIXME: other kinds of macro uses (id macro, set! macro)

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

@(define (test-my-and)
   @examples[#:eval the-eval #:hidden
   (assert (equal? (my-and (even? 2) (odd? 3) (even? 4)) #t))
   (assert (equal? (my-and (even? 2) (odd? 4) (/ 0)) #f))
   ])

How can we implement our own macro with this shape? There are three basic
implementation strategies:
@itemlist[
@item{recursive macro}
@item{recursive run-time helper function}
@item{recursive compile-time helper function}
]

@; ----------------------------------------
@subsection[#:tag "ellipses-rec-macro"]{Recursive Macros}

The first strategy is to write a macro that does case analysis and explicit
recursion --- that is, the macro expands into another use of itself. Here is a
recursive implementation of @racket[my-and]:
@examples[#:eval the-eval #:no-result
(code:comment "(my-and Expr ...) : Expr")
(define-syntax my-and
  (syntax-parser
    [(_)
     #'#t]
    [(_ e1:expr e:expr ...)
     #'(if e1 (my-and e ...) #f)]))
]
@(test-my-and)

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

@; ----------------------------------------
@subsection[#:tag "ellipses-rt-helper"]{Recursive Run-time Helper Function}

Another implementation strategy is to expand into another variable-arity form or
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
@(test-my-and)

Note the use of @racket[(lambda () e) ...] in the template. The pattern variable
@racket[e] occurred in front of ellipses in the pattern, so it must be used in
front of ellipses in the template. But the term before the ellipses in the
template isn't just @racket[e], it is @racket[(lambda () e)]. This
@racket[(lambda () HOLE)] wrapper gets copied for every instance of @racket[e]:
@racketblock[
(my-and 1 2 3)
==>
(andmap (lambda (thunk) (thunk))
        (list (lambda () 1) (lambda () 2) (lambda () 3)))
]
That is, ellipses in a syntax template act like an implicit @racket[map] over
the pattern variables.

For a frequently-used, simple macro like @racket[and], this might not be a good
implementation because of run-time overhead, but for other macros this kind of
implementation might be reasonable.

@lesson{Many macros can be decomposed into two parts: a compile-time part that
adds @racket[lambda] wrappers to handle scoping and delayed evaluation, and a
run-time part that implements the computation and behavior of the macro.}

@; ----------------------------------------
@subsection[#:tag "ellipses-ct-helper"]{Recursive Compile-time Helper Function}

The final strategy is to use a compile-time helper function, which handles the
recursion either directly or indirectly. Here is another implementation of
@racket[my-and], where the macro itself is not recursive but the transformer
uses a recursive compile-time helper function:

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
@(test-my-and)

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

The compile-time helper function could be written more concisely using
@racket[foldr]:
@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (code:comment "my-and-helper : (Listof Syntax[Expr]) -> Syntax[Expr]")
  (define (my-and-helper exprs)
    (foldr (lambda (expr_ rec-code)
             #`(if #,expr_ #,rec-code #f))
           #'#t
           exprs)))
]
Or we could just use the body of the compile-time helper function (now that we
have eliminated the recursion) directly in the macro:
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
@(test-my-and)

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

Ellipses can also be used with compound shapes. For example, here is the shape
of a simplified version of @racket[cond] (it doesn't support @racket[=>] and
@racket[else] clauses):

@codeblock{
;; (my-cond [Expr Expr] ...) : Expr
}

@(define (test-my-cond1)
   @examples[#:eval the-eval #:hidden
   (assert (equal? (my-cond [(even? 1) 'no] [(odd? 3) 'yes]) 'yes))
   (assert (equal? (my-cond [(even? 2) 'ok] [(/ 0) 'whoops]) 'ok))
   (assert (equal? (my-cond [#f 1] [#f 2]) (void)))
   ])

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
@(test-my-cond1)

Here is an implementation using a recursive run-time helper function:
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
@(test-my-cond1)

Here is an implementation using a recursive helper @emph{macro} --- Racket's
variadic @racket[or] macro. It also relies on fact that @racket[and] and
@racket[or] treat any value other than @racket[#f] as true, and return that
specific true value as their result when appropriate.

@examples[#:eval the-eval #:no-result
(code:comment "(my-cond [Expr Expr] ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_ [condition:expr result:expr] ...)
     #'((or (and condition (lambda () result))
            ...
            void))]))
]
@(test-my-cond1)


@exercise[#:tag "compound:cond-ct"]{Implement @racket[my-cond] using a
compile-time helper function that takes a list of condition expressions and a
list of result expressions:
@codeblock{
;; my-cond-helper : (Listof Syntax[Expr]) (Listof Syntax[Expr]) -> Syntax[Expr]
;; PRE: the two lists of expressions have the same length
}
Hint: Racket's @racket[foldr] function is variadic.}

@exercise[#:tag "compound:my-and-let2"]{Generalize @racket[my-and-let] so that
it takes a list of identifier-and-expression clauses. That is, it should have
the following shape:
@codeblock{
;; (my-and-let ([Id Expr] ...) Expr) : Expr
}
The scope of each identifier includes all subsequent clauses and the final
expression. Implement the macro either as a recursive macro or by using a
compile-time helper function.}

@exercise[#:tag "compound:my-evcase1"]{Design a macro @racket[my-evcase1] with
the following shape:
@codeblock{
;; (my-evcase1 Expr [Expr Expr] ...) : Expr
}
The macro evaluates its first argument to get the value to match. Then it tries
each clause until one is selected. A clause is selected if its first expression
produces a value equal to the value to match; that clause's second expression is
the result of the macro. If no clause matches, the result is @racket[(void)].

@racketblock[
(my-evcase1 (begin (printf "got a coin!\n") (* 5 5))
  [5 "nickel"]
  [10 "dime"]
  [25 "quarter"]
  [(/ 0) "infinite money!"])
(code:comment "expect print once, result = \"quarter\"")
]}

@(close-eval the-eval)
