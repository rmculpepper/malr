#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base))

@(define the-eval (make-base-eval))

@title[#:tag "part-basic"]{Basic Macrology}
@author["Ryan Culpepper" "Claire Alvis"]

@section[#:tag "basic-first"]{Your First Macro}

@(declare-keyword assert)

Let's write a macro, @racket[assert], that takes an expression and
evaluates it, raising an error if it does not evaluate to a true
value.

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
but we can instead make it generate code that will produce the right
error at run time, with the help of the @racket[quote] form and
@racket[error]'s built in formatting capabilities:

@racketblock[
(unless (>= (length ls) 1)
  (error 'assert "assertion failed: ~s" (quote (>= (length ls) 1))))
]

So we write the macro as follows:

@racketblock+eval[#:eval the-eval
(define-syntax-rule (assert expr)
  (unless expr
    (error 'assert "assertion failed: ~s" (quote expr))))
]

When we use a macro like

@racketblock[
(assert (>= (length ls) 1))
]

the macro expander substitutes the argument @racket[(>= (length ls)
1)] for every occurrence of the pattern variable @racket[e] in the
macro definition's template---including the occurrence within the
@racket[quote] form:

@racketblock[
(unless (>= (length ls) 1)
  (error 'assert "assertion failed: ~s"
         '(>= (length ls) 1)))
]

@lesson{It's often simpler to produce an expression that does a
computation at run time than to do the computation at compile time.}

@exercise{Write a macro @racket[noisy-v1] that takes an expression
@racket[_expr] and prints @racketvalfont{"evaluating
@racket[_expr]\n"} before evaluating the expression. The result of the
macro should be the result of the expression. (Hint: use
@racket[begin].)}

@exercise{Write a macro @racket[noisy-v2] that takes an expression
@racket[_expr] and prints @racketvalfont{"evaluating @racket[_expr]
..."} before evaluating the expression and @racketvalfont{"done\n"}
afterwards. The result of the macro should be the result of the
expression. (Hint: use @racket[begin0].)}


@section[#:tag "basic-or2"]{Auxiliary Variables and Hygiene}

@(declare-keyword or2)

Suppose we want a macro @racket[or2] that expects two expressions
@racket[e1] and @racket[e2] and returns the first true expression
or @racket[#f].  Here is a first attempt at defining of @racket[or2]:

@racketblock[
(define-syntax-rule (or2 e1 e2)
  (if e1 e1 e2))
]

If @racket[e1] is a simple expression like @racket[#t], this
definition will work just fine, but if is a complex expression that
produces a true value, it will be evaluated twice. Worse, if it
contains side-effects, the side-effects will be performed twice. 

@lesson{A macro template should contain at most one reference to an
expression argument.}

So instead we should evaluate @racket[e1] first and save the result in
a temporary variable @racket[x]:

@racketblock+eval[#:eval the-eval
(define-syntax-rule (or2 e1 e2)
  (let ([x e1])
    (if x x e2)))
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
of its second argument.

One cause for concern is the use of @racket[x] as an auxiliary
variable. Might this use of @racket[x] interfere with a use of
@racket[x] in the expressions we give to @racket[or2]?

For example, consider this use of the macro:

@racketblock[
(let ([x 5])
  (or2 (even? x) (odd? x)))
]

If we do this expansion by hand, we would expect to get the following:

@interaction[#:eval the-eval
(let ([x 5])
  (let ([x (even? x)])
    (if x x (odd? x))))
]

But when we use the macro, it---surprisingly---behaves exactly as we
would have wanted! 

@interaction[#:eval the-eval
(let ([x 5])
  (or2 (even? x) (odd? x)))
]

That is, the occurrence of @racket[x] in @racket[(odd? x)] refers to
the binding of @racket[x] around the @emph{use} of the macro, not the
binding introduced by its @emph{expansion}.

This property of Racket macros is called @emph{hygiene}. Instead of
the ``naive'' expansion we wrote above, the Racket macro expander
actually produces something like the following:

@racketblock[
(let ([x 5])
  (let ([x_1 (even? x)])
    (if x_1 x_1 (odd? x))))
]

The macro expander distinguishes identifiers introduced by a macro and
keeps them ``separate'' from identifiers given to the macro in its
arguments. If an introduced identifier is used in a binding position,
it does @emph{not} capture identifiers of the same name in the macro's
arguments.

Similarly, references introduced by the macro are not captured by
bindings of the same name in the context of the macro's use. In the
example above, the references to the @racket[let] and @racket[if]
syntactic forms refer to the @racket[let] and @racket[if] bindings in
scope at the macro definition site, regardless of what those names
mean at the macro use site.

We will discuss the actual mechanism the macro expander uses to
enforce hygiene later in the guide.

@lesson{An identifier that is part of a macro template neither
captures references in a macro argument, if the identifier is used as
a binder, nor is it captured by bindings in the environment where the
macro is used, if the identifier is used as a reference.}


@section[#:tag "basic-binding-forms"]{Binding Forms}

One of the most powerful and unique capabilities of macros is the
ability to create new binding forms---macros that evaluate expressions
in an environment extended with additional bindings.

A binding form accepts identifiers to bind, in addition to
expressions, as arguments. (Hygiene prevents only identifiers present
as literals in the macro template from binding references in macro
arguments---binders that come from macro arguments do bind references
in other macro arguments.)

For example, consider the @racket[andlet1] macro, which takes an
identifier (binder) and two expressions. The first expression is
evaluated in the environment of the macro use, without extensions. If
it produces a true value, the second is evaluated in that environment
extended with the identifier bound to the value of the first
expression. In other words, the scope of the identifier is the second
expression.

@racketblock+eval[#:eval the-eval
(define-syntax-rule (andlet1 x e1 e2)
  (let ([x e1])
    (if x e2 #f)))
]

By inspecting the macro template, we can see that @racket[e2] is in
the scope of the @racket[let]-binding of @racket[x], and @racket[e1]
is not.

@exercise{Write a macro @racket[iflet] that takes an identifier and
three expressions. If the first expression (the condition) evaluates
to a true value, that value is bound to the identifier and the second
expression (the ``then branch'') is evaluated in its scope; otherwise,
the third expression is evaluated @emph{outside the scope of the
identifier}.

@racketblock[
(define alist '((1 . apple) (2 . pear)))
(equal? (iflet x (assoc 1 alist) (cdr x) 'none) 'apple)
(equal? (let ([x 'plum]) (iflet x (assoc 3 alist) (cdr x) x)) 'plum)
]

@;{
;; one solution:
(define-syntax-rule (iflet x e1 e2 e3)
  (let ([tmp e1])
    (if tmp
        (let ([x tmp]) e2)
        e3)))
}
}


@section[#:tag "basic-dynamic"]{Changing an Expression's Dynamic Context}

@(declare-keyword capture-output)

So far, we've seen a few things that a macro can do with an expression
argument. It can use its value (as in @racket[assert]); it turn it
into a datum using @racket[quote]; it can extend the environment the
expression is evaluated in; and it can decide whether or not to
evaluate it (as in the short-circuiting @racket[or2]).

Another thing a macro can do is affect the @emph{dynamic context} an
expression is evaluated in. For now, we'll use @tech/guide{parameters}
as the primary example of dynamic context, but others include threads,
continuation marks, and @racket[dynamic-wind].

For example, consider a macro that evaluates its argument expression,
throws away the value, and returns a string representing all of the
output generated during the expression's evaluation.

@racketblock+eval[#:eval the-eval
(define-syntax-rule (capture-output expr)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      expr
      (get-output-string out))))
]

@interaction[#:eval the-eval
(printf "hello world!")
(capture-output (printf "hello world!"))
]

@exercise{Write a macro @racket[handle] that takes two expressions. It
should evaluate the first expression, and if it returns a value, the
result of the macro use is that value. If the first expression raises
an exception, it evaluates the second expression and returns its
result. Use @racket[with-handlers].

@racketblock[
(equal? (handle 5 6) 5)
(equal? (handle (/ 1 0) 'whoops) 'whoops)
]
}


@section[#:tag "basic-minimize"]{Minimizing Macros}

Recall @racket[capture-output] from @secref["basic-dynamic"]. The only
reason it needs to be a macro at all is to delay the evaluation of its
argument until it can place it in the proper context. Another way to
implement @racket[capture-output] is for the macro itself to only
delay the evaluation of the expression---by turning it into a
procedure---and rely on a function that implements the dynamic
behavior.

@racketblock+eval[#:eval the-eval
(define-syntax-rule (capture-output e)
  (capture-output-fun (lambda () e)))
(code:comment "capture-output-fun : (-> Any) -> String")
(define (capture-output-fun thunk)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      (thunk)
      (get-output-string out))))
]

The benefit of factoring out the parameterization is twofold: it
minimizes the size of the expanded code, and it allows you to test the
helper function @racket[capture-output-fun].

@lesson{Keep the code introduced by a macro to a minimum. Rely on
helper functions to implement complex dynamic behavior.}

@exercise{Rewrite the @racket[handle] macro so that the dynamic
behavior is implemented by a function.}

@exercise{Write a macro @racket[forever] that takes an expression and
evaluates it repeatedly in a loop. Write a helper function to
implement the dynamic behavior.}

@exercise{Rewrite the @racket[andlet1] macro so that the dynamic
behavior is implemented by a function. What happens to the identifier
argument? (Note: this macro is so simple that there is no benefit to
creating a separate function to handle it. Do it anyway; it's an
instructive example.)}


@; ============================================================

@section[#:tag "basic-ellipses"]{Ellipsis Patterns and Templates}

Let us continue with the @racket[capture-output] macro, and let us
suppose that we want to extend it to take multiple expressions, which
should be evaluated in order, and the output of all of them captured
and combined.

The macro system we are using gives us a convenient way to represent a
sequence of arbitrarily many expressions.  We indicate that a macro
accepts an arbitrary number of arguments with a @racket[...] in the
pattern after the pattern variable.  Then, in the template, we use
@racket[...] after the code that contains the pattern variable---in
this case, just the pattern variable itself.

@racketblock+eval[#:eval the-eval
(define-syntax-rule (capture-output e ...)
  (capture-output-fun (lambda () e ...)))
]

@interaction[#:eval the-eval
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

@racketblock+eval[#:eval the-eval
(define-syntax-rule (capture-output e ...)
  (capture-output-fun (lambda () e ... (void))))
]
@interaction[#:eval the-eval
(capture-output)
]

Alternatively, the macro-writer might require @racket[capture-output]
be called with at least one argument. Then, the
@racket[(capture-output)] call will give a more meaningful error
message, as seen below.

@racketblock+eval[#:eval the-eval
(define-syntax-rule (capture-output e e* ...)
  (capture-output-fun (lambda () e e* ...)))
]
@interaction[#:eval the-eval
(capture-output)
]

Yet another option is to wrap each expression individually and pass a
list of functions to the auxiliary function:

@racketblock[
(define-syntax-rule (capture-output e ...)
  (capture-output-fun (list (lambda () e) ...)))

(define (capture-output-fun thunks)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      (for ([thunk (in-list thunks)]) (thunk))
      (get-output-string out))))
]

@exercise{Write @racket[my-and] and @racket[my-or] macros that take
arbitrary numbers of expressions.}


@section[#:tag "basic-pattern-dots"]{Ellipses with Complex Patterns}

Consider a simplified version of Racket's @racket[let] form with the
following syntax:

@defform[#:link-target? #f
         #:literals (else)
         (my-let ([id rhs-expr] ...) body-expr)]{}

We can use ellipses after a complex pattern, not just after a simple
pattern variable:

@racketblock[
(define-syntax-rule (my-let ([id rhs-expr] ...) body-expr)
  ((lambda (id ...) body-expr) rhs-expr ...))
]

We can also implement @racket[my-letrec], which has the same syntax as
@racket[my-let] but evaluates all of the right-hand side expressions
in the scope of all of bound variables.

@racketblock[
(define-syntax-rule (my-letrec ([id rhs-expr] ...) body-expr)
  (my-let ([id #f] ...)
    (set! id rhs-expr) ...
    body-expr))
]

Now consider a simplified version of Racket's @racket[cond] form with
the following syntax:

@defform[#:link-target? #f
         (my-cond [question-expr answer-expr] ...)]{}



@racketblock[
(define-syntax-rule (my-cond [question-expr answer-expr] ...)
  (my-cond-fun (list (lambda () question-expr) ...)
               (list (lambda () answer-expr) ...)))
]





@section[#:tag "basic-rec"]{Recursive Macros}

Let's think about how to write a simplified version of Racket's
@racket[cond] form. Here's the syntax:

@defform[#:link-target? #f
         #:literals (else)
         (mycond clause ...)
         #:grammar ([clause [test-expr answer-expr]
                            [else answer-expr]])]

But there's an additional constraint: only the last clause of the
@racket[mycond] expression can be an @racket[else] clause. A more
precise specification of the syntax is the following:

@defform[#:link-target? #f
         #:literals (else)
         (mycond clause ... maybe-final-clause)
         #:grammar ([clause [test-expr answer-expr]]
                    [maybe-final-clause (code:line)
                                        [else answer-expr]])]

Using only @racket[define-syntax-rule], even with ellipses, we can't
distinguish the final clause. We need a more 

(If not for the final clause, we 

@racketblock[
(syntax-rules (literal-id ...)
  [pattern template] ...)
]

or, less elegantly:

@defform[#:link-target? #f
         #:literals (else)
         (mycond . clauses)
         #:grammar ([clauses ()
                             ([else answer-expr])
                             ([test-expr answer-expr] . clauses)])]



@; ============================================================

@;{
recursive macros
 - cond

recursive macros w/ multiple variants
 - cond w/ =>, else
 - also intros identifier-subforms
 - also, syntax grammars ???

expansion contexts
 - forward ref to "we'll show you later how to create more contexts"
 - discovery process

recap and misc:
 - Macros are rewrite rules, outward in, discovery process.
 - new: Don't use a macro when a function would suffice.
 - new: Not every macro needs to be written.

}




@(close-eval the-eval)



