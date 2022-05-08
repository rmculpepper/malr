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
@section[#:tag "shape:list"]{List and Ellipsis Shapes}

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

We can also have compound shapes in front of ellipses. For example, here is the
shape of a simplified version of @racket[cond] (it doesn't support @racket[=>]
and @racket[else] clauses):

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
    [(_ [condition1:expr result1:expr] clause ...)
     #'(if condition1
           result1
           (my-cond clause ...))]))
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
