;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/port rackunit))

@(define the-eval (make-malr-eval))
@(the-eval '(require (for-syntax racket/match)))

@; ============================================================
@title[#:tag "enum-shapes" #:version ""]{Enumerated Shapes}

This section introduces enumerated shapes --- that is, shapes with multiple
variants. An enumerated shape poses a problem in defining the interface between
the corresponding syntax class and the macro that uses it. This section
discusses several strategies for defining this interface.


@; ------------------------------------------------------------
@section[#:tag "enum-shapes-def"]{Defining Enumerated Shapes}

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

@(define (test-my-cond2)
   @examples[#:eval the-eval #:hidden
   (define ls '((a 1) (b 2) (c 3)))
   (assert (equal? (my-cond [(assoc 'b ls) #:apply cadr] [#t 0]) 2))
   (assert (equal? (my-cond [(assoc 'z ls) #:apply cadr] [#t 0]) 0))
   ])

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

The interface between syntax class and macro is determined by how we allocate
reponsibility between the two for the interpretation of the syntax class's
terms. This problem is a fundamentally difficult one, and in general there is no
single right answer, but there are some standard @deftech{interface strategies}:
@itemlist[

@item{empty interface (redo case analysis)}
@item{common meaning}
@item{macro behavior}
@item{code generation}
@item{AST}

]
Each has different tradeoffs, and some don't work in all situations. The
following sections discuss each approach in greater detail.

@; ----------------------------------------
@subsection[#:tag "enum-empty"]{Empty Interface Strategy (Redo Case Analysis)}

One option is to give the syntax class no responsibility for interpreting its
terms, and simply redo the case analysis in the macro. This is the
@deftech{empty interface} strategy. The syntax class is still useful for input
validation and as internal documentation, but since it performs no
interpretation, we should declare that it exports no attributes.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes ()
    (pattern [condition:expr result:expr])
    (pattern [condition:expr #:apply get-result:expr])))
]

The following version of @racket[my-cond] checks the syntactic structure of all
of its arguments, then expands into a private recursive helper macro
@racket[my-cond*], which performs the case analysis on each clause again:

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
    [(_ [condition:expr result:expr] more ...)
     #'(if condition
           result
           (my-cond* more ...))]
    [(_ [condition:expr #:apply get-result:expr] more ...)
     #'(let ([condition-value condition])
         (if condition-value
             (get-result condition-value)
             (my-cond* more ...)))]))
]
@(test-my-cond2)

An advantage of this strategy is that it is nearly always a viable option. A
disadvantage is that it duplicates syntax patterns, which introduces the
possibility of discrepancies between the syntax class and the macro
clauses. Such discrepancies can lead to problems that are difficult to catch and
debug.

@exercise[#:tag "enum-cond/empty"]{Extend the definition of @shape{CondClause}
with one more variant as follows:
@codeblock{
;; CondClause ::= ... | [Expr #:bind c:Id Expr{c}]
}
If the condition evaluates to a true value, it is bound to the given variable
name and the result expression is evaluated in the scope of that variable. The
scope of the variable does not include any other clauses.

Update the design of @racket[cond-clause] and @racket[my-cond] for the new
@shape{CondClause} variant using the strategy described in this section.}

@; ----------------------------------------
@subsection[#:tag "enum-meaning"]{Common Meaning Interface Strategy}

In some cases, it is possible to find a @deftech[#:key "common meaning"]{common
meaning} shared by all of the variants that is also sufficient for the macro to
work with. In the case of @shape{CondClause}, this is relatively
straightforward: We can convert any ``normal'' clause into an ``apply'' clause
by wrapping it in a function that ignores its argument. For example, instead of
writing
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
form. We don't need to actually rewrite the whole clauses; instead, we can
change the attributes of @racket[cond-clause] to @racket[condition] and
@racket[get-result] to represent the second form, and we can change the first
form to @emph{compute} the @racket[get-result] attribute using
@racket[#:with]. Here is the new syntax class:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class cond-clause
    #:attributes (condition    (code:comment "Expr[(U #f C)]")
                  get-result)  (code:comment "Expr[C -> Any]")
    (pattern [condition:expr result:expr]
             #:with get-result #'(lambda (ignore) result))
    (pattern [condition:expr #:apply get-result:expr])))
]
I've also added comments with shape annotations for the attributes, to help me
remember their intended interpretation.

Here is an implementation of @racket[my-cond] using this version of
@racket[cond-clause] and its attributes:
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
@(test-my-cond2)

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

@exercise[#:tag "enum-cond/meaning"]{See @exercise-ref["enum-cond/empty"] for a
revised definition of @shape{CondClause}. Update the design of
@racket[cond-clause] and @racket[my-cond] for the new @shape{CondClause} variant
using the strategy described in this section.}


@; ----------------------------------------
@subsection[#:tag "enum-behavior"]{Macro Behavior Interface Strategy}

@; @bold{Option 2: Shift more of the macro's behavior into the syntax class.}

If it is difficult to find a common interface for all of a syntax class's
variants based solely on their contents, an alternative is to design the
interface based on the @deftech{macro behavior}. This is similar to shifting
from ``functional'' style operations defined separately from a data type to
``object-oriented'' style methods where behavior is defined together with the
type and its variants. The potential downside, of course, is that it couples the
syntax class more tightly with the macro.

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
             #:with code #'(lambda (fail_)
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
@(test-my-cond2)

Again, you might worry that the use of @racket[lambda] leads to run-time
inefficiency, but the way this macro uses @racket[lambda] is easily optimized
away by the compiler.

@; FIXME: verify the compiler inlines all lambdas away

@exercise[#:tag "enum-cond/behavior"]{See @exercise-ref["enum-cond/empty"] for a
revised definition of @shape{CondClause}. Update the design of
@racket[cond-clause] and @racket[my-cond] for the new @shape{CondClause} variant
using the strategy described in this section.}


@; ------------------------------------------------------------
@subsection[#:tag "enum-codegen"]{Code Generator Interface Strategy}

Suppose, though, that we really wanted to produce more natural looking code,
perhaps for readability. Here's a variation on the previous solution: Instead of
exporting a syntax-valued attribute that takes a run-time failure continuation,
export a @emph{function-valued} attribute that takes a compile-time failure
@emph{expression} and produces an expression implementing @racket[my-cond]'s
behavior for that clause. That is, the attribute represents a @deftech{code
generator} for the clause. For example:
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
Note that the @racket[make-code] attribute is declared with a type, not a shape,
and it is defined using @racket[#:attr] instead of @racket[#:with].

Here is the corresponding definition of @racket[my-cond]:
@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_)
     #'(void)]
    [(_ c:cond-clause more ...)
     ((datum c.make-code) #'(my-cond more ...))]))
]
@(test-my-cond2)
The value of @racket[c.make-code] is not syntax, so we cannot use it in a syntax
template. We use @racket[datum] to get the attribute value (a function), and
apply it to syntax representing an expression handling the rest of the clauses.

Here's another version of the macro that checks all of the clauses first and
then uses @racket[foldr] to process all of their code generators:
@examples[#:eval the-eval #:no-result
(code:comment "(my-cond CondClause ...) : Expr")
(define-syntax my-cond
  (syntax-parser
    [(_ c:cond-clause ...)
     (foldr (lambda (make-code rec-expr)
              (make-code rec-expr))
            #'(void)
            (datum (c.make-code ...)))]))
]
@(test-my-cond2)
The expression @racket[(datum (c.make-code ...))] has type @type{(Listof
(Syntax[Expr] -> Syntax[Expr]))}.

@exercise[#:tag "enum-cond/codegen"]{See @exercise-ref["enum-cond/empty"] for a
revised definition of @shape{CondClause}. Update the design of
@racket[cond-clause] and @racket[my-cond] for the new @shape{CondClause} variant
using the strategy described in this section.}

@; ----------------------------------------
@subsection[#:tag "enum-ast"]{AST Interface Strategy}

The @deftech{AST strategy} is a variation on the @secref["enum-empty"] approach,
which has the macro redo the syntax class's case analysis. But in this
variation, instead of the macro doing case analysis on the syntax, the syntax
class parses its terms into values in some AST datatype, and then the macro does
case analysis on the AST values.

This results in a larger interface between the syntax class and the macro or
macros that use it, because the interface includes the AST datatype
definition. On the other hand, the syntax class does not have to specialize its
interpretation based on the behavior of any specific macro. Furthermore, the
case analysis performed by the macro can be simpler and faster, and errors will
be easier to catch and debug, compared to discrepancies between syntax class and
macro syntax patterns.

Here is a definition of an AST datatype (@type{ClauseRep}) and a version of the
@racket[cond-clause] syntax class that exports a single @racket[ast] attribute
containing a @type{ClauseRep} value:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (code:comment "A ClauseRep is an instance of one of the following:")
  (struct clause:normal
    (condition    (code:comment "Syntax[Expr]")
     result))     (code:comment "Syntax[Expr]")
  (struct clause:apply
    (condition    (code:comment "Syntax[Expr[(U #f C)]")
     get-result)) (code:comment "Syntax[Expr[C -> Any]]")

  (define-syntax-class cond-clause
    #:attributes (ast) (code:comment "ClauseRep")
    (pattern [condition:expr result:expr]
             #:attr ast (clause:normal #'condition #'result))
    (pattern [condition:expr #:apply get-result:expr]
             #:attr ast (clause:apply #'condition #'get-result))))
]

The type @type{ClauseRep} represents parsed terms of the shape
@shape{CondClause}. I've given them separate names here for clarify, but in
practice I often use the same name for type and shape. The context usually
disambiguates them.

Here is one implementation of the @racket[my-cond] macro, using @racket[match]
to do case analysis on the clause ASTs:
@codeblock{
;; (my-cond CondClause ...) : Expr
(define-syntax my-cond
  (syntax-parser
    [(_ c:cond-clause ...)
     (foldr (lambda (ast rec-code)
              (match ast
                [(clause:normal condition result)
                 #`(if #,condition #,result #,rec-code)]
                [(clause:apply condition get-result)
                 #`(let ([condition-value #,condition])
                     (if condition-value (#,get-result condition-value) #,rec-code))]))
            #'(void)
            (datum (c.ast ...)))]))
}
@(test-my-cond2)

@exercise[#:tag "enum-cond/ast"]{See @exercise-ref["enum-cond/empty"] for a
revised definition of @shape{CondClause}. Update the design of
@racket[cond-clause] and @racket[my-cond] for the new @shape{CondClause} variant
using the strategy described in this section. Double-check that a
@racket[#:bind]-clause variable is not visible in subsequent clauses!}


@; ------------------------------------------------------------
@section[#:tag "enum-overlap"]{Designing Enumerated Syntax}

When you design an enumerated syntax shape, you must avoid ambiguity; or if you
cannot completely avoid it, you must manage it carefully. To elaborate, let's
consider some alternative syntaxes for cond-clauses.

For @shape{AltCondClauseV1}, let's generalize the simple form, so that the result
is determined not by a single expression but by one or more body terms. And
let's indicate the second form with the identifier @racket[apply] instead of the
keyword @racket[#:apply].

@codeblock{
;; AltCondClauseV1 ::=
;; | [Expr Body ...+]
;; | [Expr apply Expr]
}

This syntax design is @emph{bad}, because there are two variants with different
meanings that contain the same terms. In fact, every term that matches the
second variant also matches the first.

One could argue that a programmer is unlikely to simply write @racket[apply] at
the beginning of a result body intending it to be evaluated as an expression. It
would have no effect; its presence would be completely useless. Still,
programmers regularly trip on ``out of the way'' inconsistencies, and it's a
better habit to keep comfortable safety margins.

Let's change the definition slightly so that instead of @racket[apply], we use
the identifier @racket[=>] to indicate the second clause form:

@codeblock{
;; AltCondClauseV2 ::=
;; | [Expr Body ...+]
;; | [Expr => Expr]
}

This syntax design is okay; it is in fact the design Racket uses for
@racket[cond] clauses. There are two crucial details, though. First, the
@racket[=>] variant must be recognized not by the symbolic content of the
@racket[=>] identifier, but by checking whether it is a reference to Racket's
@racket[=>] binding. Second. Racket defines @racket[=>] as a macro that always
signals a syntax error. So even though we can interpret @racket[=>] as an
expression, it is never a @emph{valid} expression. In practice, we only care
about avoiding overlaps with the set of valid expressions, so
@shape{AltCondClauseV2} is okay.

Both properties are needed to avoid ambiguity. If we checked for @racket[=>] as
a symbol, then a programmer could define a local variable (or macro) named
@racket[=>], which would then be a valid expression, so there would be
overlap. And if @racket[=>] were a valid expression, that also creates an
overlap. (That's the problem with the @racket[apply] variant in
@shape{AltCondClauseV1}.)

Finally, although the shape is okay, we must be careful when writing the
corresponding syntax patterns. First, we must declare @racket[=>] as a
@emph{literal}; otherwise it would be treated as another pattern variable. Here
is a first draft of the corresponding syntax class definition, based on the
``macro behavior interface'' strategy:
@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class alt-cond-clause-v2
    #:attributes (code) (code:comment "Expr[(-> Any) -> Any]")
    #:literals (=>)
    (pattern [condition:expr result:expr ...+]
             #:with code ELIDED)
    (pattern [condition:expr => get-result:expr]
             #:with code ELIDED)))
]
The problem is that a clause term like @racket[[a => b]] matches the first
pattern, and so the syntax class interprets it as a simple condition and
result-body clause. That is, the same issue we dealt with earlier at the shape
level shows up again at the syntax pattern level. It shows up again because even
though @racket[=>] cannot be a valid expression, the @racket[expr] syntax class
doesn't know that. This issue is not specific to syntax classes; it would also
come up if we did the case analysis in the macro patterns.

The solution involves two steps. First, reorder the patterns to put the
@racket[=>] pattern first. Second, use @racket[~!] (``cut'') to commit to the
first pattern after seeing @racket[=>]. Here is the code:

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class alt-cond-clause-v2
    #:attributes (code) (code:comment "Expr[(-> Any) -> Any]")
    #:literals (=>)
    (pattern [condition:expr => ~! get-result:expr]
             #:with code ELIDED)
    (pattern [condition:expr result:expr ...+]
             #:with code ELIDED)))
]

Without the @racket[~!], a term like @racket[[a => b c]] would be considered a
valid instance of the second pattern, rather than an invalid instance of the
first pattern. (Try it and see what happens!)

The complexity of overlaps with expressions is one reason that keywords were
introduced into Racket. Since both the @shape{Expr} shape and the @racket[expr]
syntax class consider themselves completely disjoint from keywords, they avoid
these issues completely. (A related issue does emerge when dealing with
partly-expanded code, distinguishing definitions from expressions, for
example. We'll talk about that later. FIXME-REF)

@; also FIXME-REF for local-expand, eg define/public and public

@(close-eval the-eval)
