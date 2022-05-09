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
@title[#:tag "enum-shapes" #:version ""]{Enumerated Shapes}

In the previous section, we extracted the definition of @shape{CondClause} from
the shape of the @racket[my-cond] macro, and we defined a syntax class
@racket[cond-clause] corresponding to the shape.

Now let's extend @shape{CondClause} with another @emph{variant} that allows the
clause's result to depend on the (true) value produced by the condition. This is
similar to the @racket[=>] clause form that Racket's @racket[cond] macro
supports, but we'll use a keyword, @racket[#:apply], to distinguish this form of
clause for @racket[my-cond]. Here is the updated shape definition:

@codeblock{
;; CondClause ::=
;; | [Expr Expr]          -- represents condition, result
;; | [Expr #:apply Expr]  -- represents condition, function from condition to result
}

Here are some examples:
@racketblock[
(define ls '((a 1) (b 2) (c 3)))
(my-cond [(assoc 'b ls) #:apply cadr] [#t 0])
(code:comment "expect 2")
(my-cond [(assoc 'z ls) #:apply cadr] [#t 0])
(code:comment "expect 0")
]

Here is one way the first example could expand (just the first step):
@racketblock[
(let ([condition-value (assoc 'b ls)])
  (if condition-value
      (cadr condition-value)
      (my-cond [#t 0])))
]

We update the definition of @racket[cond-clause] by adding another
@racket[pattern] clause for the new variant. Its second expression has a
different intepretation, so we should use a different name for its pattern
variable so that we don't confuse them:

@racketblock[
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes (condition result) (code:comment "!!")
    (pattern [condition:expr result:expr])
    (pattern [condition:expr #:apply get-result:expr])))
]

There's a problem, though. The new pattern is fine by itself, but it doesn't fit
with the existing @racket[#:attributes] declaration. The second variant doesn't
have a simple @racket[result] expression; it interprets its second expression
differently. The syntax class, though, needs a single interface that determines
what nested attributes are bound when the syntax class is used in a macro and
how the nested attributes are interpreted.

This mismatch is a fundamentally difficult issue to deal with, and in general
there is no single right answer. Here are some of the options:

@section{Unify the Variants}

@bold{Option 1: Find a way to unify the two variants into a single interface.}

It is relatively straightforward to unify the two @shape{CondClause} variants
into a single interface in this example. We can convert any clause of the
first form into a clause of the second form by wrapping it in a function that
ignores its argument. For example, instead of writing
@racketblock[
(cond [(even? 2) 'e]
      [(odd? 2) 'o])
]
we could instead write
@racketblock[
(cond [(even? 2) #:apply (lambda (ignore) 'e)]
      [(odd? 2) #:apply (lambda (ignore) 'o)])
]
That is, the second clause form is strictly more general than the first clause
form. We don't need to actually rewrite the whole clauses, but we can change the
attributes of @racket[cond-clause] to @racket[condition] and @racket[get-result]
to represent the second form, and we can change the first form to @emph{compute}
the @racket[get-result] attribute using @racket[#:with]. Here is the new syntax
class:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes (condition    (code:comment "Expr[(U #f C)]")
                  get-result)  (code:comment "Expr[C -> R]")
    (pattern [condition:expr result:expr]
             #:with get-result #'(lambda (ignore) result))
    (pattern [condition:expr #:apply get-result:expr])))
]
I've also added comments with shape annotations for the attributes, to help me
remember their intended interpretation.

Now let's update the @racket[my-cond] macro's implementation to use the new
version of @racket[cond-clause] and its new attributes.

@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ c:cond-clause more ...)
     #'(let ([condition-value c.condition])
         (if condition-value
             (c.get-result condition-value)
             (my-cond more ...)))]))
]

You might worry that introducing a @racket[lambda] wrapper and a function call
for every simple clause form will make the generated code run slower. After all,
@racket[lambda] requires a closure allocation, right? In this case, that is not
true. The generated @racket[lambda] wrappers appear directly in application
position, and the Racket compiler is more than smart enough to inline those
applications away. So even though the new version of the macro expands to
different Racket code for simple clauses, the compiler produces exactly the same
compiled code, with zero run-time overhead.

@; Use LINKLET_SHOW_CP0 to demonstrate the `lambda` is compiled away!

@; Clarify type quantifiers: universal wrt parsing, existential wrt result (ish)




@section{Unify the Behavior}

@bold{Option 2: Shift more of the macro's behavior into the syntax class.}

If it is difficult to find a common interface for all of a syntax class's
variants, one thing that can help is to move behavior from the macro to the
syntax class. This is similar to shifting from ``functional'' style operations
defined separately from a data type to ``object-oriented'' style methods where
behavior is defined together with the type and its variants. The potential
downside, of course, is that it couples the syntax class more tightly with the
macro.

In this example, we can move the responsibility for testing the condition and
producing the result if the clause is selected from the macro to the syntax
class. What is left, then? If the clause is not selected (that is, the clause
``fails''), it needs to be told how to continue the search for an answer. We can
represent ``how to continue'' with a thunk of type @type{(-> Any)}; this
kind of thunk is traditionally called a ``failure continuation''. (This sense of
the word ``continuation'' does not refer to the kind of value exposed by
@racket[call/cc], etc.) So the whole clause is represented by a function that
takes a failure continuation and produces an answer; it has the type @type{(->
Any) -> Any}. We define @racket[cond-clause] with a single attribute,
@racket[code], containing an expression for that function:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes (code) (code:comment "Expr[(-> Any) -> Any]")
    (pattern [condition:expr result:expr]
             #:with code #'(lambda (fail_)
                             (if condition result (fail_))))
    (pattern [condition:expr #:apply get-result:expr]
             #:attr code #'(lambda (fail_)
                             (let ([condition-value condition])
                               (if condition-value
                                   (get-result condition-value)
                                   (fail_)))))))
]
Now in the recursive case, the @racket[my-cond] macro just calls the clause's
@racket[code] with a failure continuation that tries the rest of the clauses:
@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ c:cond-clause more ...)
     #'(c.code (lambda () (my-cond more ...)))]))
]

Again, you might worry that the use of @racket[lambda] leads to run-time
inefficiency, but the way this macro uses @racket[lambda] is easily optimized
away by the compiler.

@; FIXME: verify the compiler inlines all lambdas away

Suppose, though, that we really wanted to produce more natural looking code,
perhaps for readability. Here's a variation on the previous solution: Instead of
exporting a syntax-valued attribute that takes a run-time failure continuation,
export a @emph{function-valued} attribute that takes a compile-time failure
@emph{expression}. That is:
@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes (make-code) (code:comment "Syntax[Expr] -> Syntax[Expr]")
    (pattern [condition:expr result:expr]
             #:attr make-code (lambda (fail-expr)
                                #`(if condition result #,fail-expr)))
    (pattern [condition:expr #:apply get-result:expr]
             #:attr make-code (lambda (fail-expr)
                                #`(let ([condition-value condition])
                                    (if condition-value
                                        (get-result condition-value)
                                        #,fail-expr))))))
]
Here is the corresponding definition of @racket[my-cond]:
@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ c:cond-clause more ...)
     ((attribute c.make-code) #'(my-cond more ...))]))
]
The value of @racket[c.make-code] is not syntax, so we cannot use it in a syntax
template. We use @racket[attribute] to get the attribute value (a function), and
apply it to syntax representing an expression handling the rest of the clauses.

@section{Redo the Case Analysis}

@bold{Option 3: Throw away the syntax class.}

FIXME

A variation on this option would be to keep the syntax class but remove its
attributes and use it only for error checking in the public version of the
macro, and then perform recursion and case analysis again in an internal version
of the macro.

@; There are other options, see FIXME-REF

@; FIXME: avoid overlapping shapes, or impose consistency, or impose order
