#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base))

@(define the-eval (make-base-eval))

@title{Expressions}

@section{Your First Macro}

@(declare-keyword assert)

Let's write a macro, @racket[assert], that takes an expression and evaluates it, raising
an error if it does not evaluate to a true value.

We can use @racket[define-syntax-rule] to define very simple macros. A
@racket[define-syntax-rule] definition consists of two parts: a
pattern and a template. The pattern describes what uses of the macros
look like, and the template is used to construct the term that the
macro is rewritten to. Identifiers occuring in the pattern are called
pattern variables; the terms that they match from the macro use are
substituted into the template when the macro is rewritten.

The @racket[assert] macro should be used like

@racketblock[
(assert _expression)
]

For example, say we have a list @racket[ls], and we wish to check
whether the length of the list is greater or equal to one:

@racketblock[
(assert (>= (length ls) 1))
]

This use should behave as if we had written 

@racketblock[
(unless (>= (length ls) 1)
  (error 'assert "assertion failed: (>= (length ls) 1)"))
]

We can't (yet) actually make the macro create the exact string above,
but we can instead make it produce code that will produce the right
error at run time, with the help of the @racket[quote] form and
@racket[error]'s built in formatting capabilities:

@racketblock[
(unless (>= (length ls) 1)
  (error 'assert "assertion failed: ~s" (quote (>= (length ls) 1))))
]

So we write the macro as follows:

@racketblock[
(define-syntax-rule (assert e)
  (unless e (error 'assert "assertion failed: ~s" (quote e))))
]

When we use a macro like

@racketblock[
(assert (>= (length ls) 1))
]

the macro expander substitutes the argument @racket[(>= (length ls)
1)] for every occurrence of the pattern variable @racket[e] in the
macro definition's template---including the occurrence within the
@racket[quote] form.

FIXME: compile vs run time

@section{Auxiliary Variables and Hygiene}

@(declare-keyword or2)

Now, we want a macro @racket[or2] that expects two expressions
@racket[e1] and @racket[e2], and will return the first true expression
or @racket[#f].  Here is a first attempt at defining of @racket[or2]:

@racketblock[
(define-syntax-rule (or2 e1 e2)
  (if e1 e1 e2))
]

If @racket[e1] is a simple expression like @racket[#t], this
definition will work just fine, but if is a complex expression,
possibly containing effects, that work will be duplicated.  So, we
should evaluate @racket[e1] first and save the result in a temporary
variable @racket[t]:

@interaction[#:eval the-eval
(define-syntax-rule (or2 e1 e2)
  (let ((t e1))
    (if t t e2)))
]

If we try this macro, it seems to work as we expect:

@interaction[#:eval the-eval
(or2 #f #f)
(or2 #t #f)
(or2 5 7)
(or2 #t (/ 1 0))
]

Notice that in the final example, @racket[or2] returns @racket[#t]
without evaluating @racket[(/ 1 0)], which would have raised an
error. In other words, @racket[or2] ``short-circuits'' the evaluation
of its arguments.

One cause for concern is the use of @racket[t] as an auxiliary
variable. Might this use of @racket[t] interfere with a use of
@racket[t] in the expressions we give to @racket[or2]?

For example, consider this use of the macro:

@racketblock[
(let ((t 5))
  (or2 (even? t) (odd? t)))
]

If we do this expansion by hand, we would expect to get the following:

@interaction[#:eval the-eval
(let ((t 5))
  (let ((t (even? t)))
    (if t t (odd? t))))
]

But when we use the macro, it---surprisingly---behaves exactly as we
would have wanted! 

@interaction[#:eval the-eval
(let ((t 5))
  (or2 (even? t) (odd? t)))
]

That is, the occurrence of @racket[t] in @racket[(odd? t)] refers to
the binding of @racket[t] around the @emph{use} of the macro, not the
binding introduced by its @emph{expansion}.

This property of Racket macros is called @emph{hygiene}. Instead of
the ``naive'' expansion we wrote above, the Racket macro expander
produces something like the following:

@racketblock[
(let ((t 5))
  (let ((t_1 (even? t)))
    (if t_1 t_1 (odd? t))))
]

The macro expander distinguishes identifiers introduced by a macro and
keeps them ``separate'' from identifiers given to the macro in its
arguments. If an introduced identifier is used in a binding position,
it does @emph{not} capture identifiers of the same name in the macro's
arguments. We will discuss the actual mechanism the macro expander
uses to do this later in the guide, but for now, @emph{we can use
auxiliary variable bindings in macro templates without fear!}


@section{Changing an Expression's Dynamic Context}

@(declare-keyword capture-output)

So far, we've seen a few things that a macro can do with an expression
argument. It can use its value (as in @racket[assert]); it turn it
into a datum using @racket[quote]; and it can decide whether or not to
evaluate it (as in the short-circuiting @racket[or2]).

Another thing a macro can do to an expression is affect the
@emph{dynamic context} it is evaluated in. For now, we'll use
@tech{parameters} as the primary example of dynamic context, but
others include threads, continuation marks, and @racket[dynamic-wind].

For example, consider a macro that evaluates its argument expression,
throws away the value, and returns a string representing all of the
output generated during the expression's evaluation.

@interaction[#:eval the-eval
(define-syntax-rule (capture-output e)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      e
      (get-output-string out))))
]

@interaction[#:eval the-eval
(printf "hello world!")
(capture-output (printf "hello world!"))
]

The only reason @racket[capture-output] needs to be a macro at all is
that it needs to delay the evaluation of its argument until it can
place it in the proper context. Another way to implement
@racket[capture-output] is for the macro itself to only delay the
evaluation of the expression and rely on a function that implements
the dynamic behavior.

@interaction[#:eval the-eval
(define-syntax-rule (capture-output e)
  (capture-output* (lambda () e)))

(code:comment "capture-output* : (-> Any) -> String")
(define (capture-output* thunk)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      (thunk)
      (get-output-string out))))
]

The benefit of factoring out the parameterization is twofold: it
minimizes the expanded code, and allows the macro-writer to
individually test the function @racket[capture-output*].

@section{Ellipses}

If we have multiple expressions and want the output of all of them, we
have a few options. We could use @racket[capture-output] on each and
append the results. Or we could join the expressions into a single
expression using @racket[begin]. But it would be convenient if our
@racket[capture-output] macro could accept multiple expressions,
evaluate each in order, and return the output of all of the
expressions. 

The macro system we are using gives us a convenient way to represent a
sequence of arbitrarily many expressions.  We indicate that a macro
accepts an arbitrary number of arguments with a @racket[...] in the
pattern after the pattern variable.  Then, in the template, we use
@racket[...]  after the code that contains the pattern variable---in
this case, just the pattern variable itself.

@interaction[#:eval the-eval
(define-syntax-rule (capture-output e ...)
  (capture-output* (lambda () e ...)))
(capture-output
  (displayln "I am the eggman")
  (displayln "They are the eggmen")
  (displayln "I am the walrus"))
]

Unfortunately, this implementation breaks when @racket[capture-output]
is called with no arguments.

@interaction[#:eval the-eval
(capture-output)
]

When @racket[capture-output] is called with no arguments, it produces
a @racket[lambda] with an empty body, which is illegal.  One way to
fix this is to insert a final expression within the @racket[lambda]:

@interaction[#:eval the-eval
(define-syntax-rule (capture-output e ...)
  (capture-output* (lambda () e ... (void))))
(capture-output)
]

Alternatively, the macro-writer might require @racket[capture-output]
be called with at least one argument. Then, the
@racket[(capture-output)] call will give a more meaningful error
message, as seen below.

@interaction[#:eval the-eval
(define-syntax-rule (capture-output e e* ...)
  (capture-output* (lambda () e e* ...)))
(capture-output)
]

Yet another option is to wrap each expression individually and pass a
list of functions to the auxiliary function:

@interaction[#:eval the-eval
(define-syntax-rule (capture-output e ...)
  (capture-output* (list (lambda () e) ...)))

(define (capture-output* thunks)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      (for ([thunk (in-list thunks)]) (thunk))
      (get-output-string out))))
]

@(close-eval the-eval)
