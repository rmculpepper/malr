;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string racket/struct-info syntax/transformer racket/syntax
                     racket/promise racket/contract racket/list
                     rackunit syntax/parse/experimental/template))

@(define the-eval (make-malr-eval))
@(the-eval '(require racket/match racket/string racket/promise
                     (for-syntax racket/list syntax/transformer)))

@; ============================================================
@title[#:tag "reinterpret-body"]{Reinterpreting Body Terms}



@; ------------------------------------------------------------
@section[#:tag "partial-expand"]{Partial Expansion}

Let's design a macro @racket[hash-from-definitions], which takes a list of body
terms and produces a hash whose entries correspond to the variable definitions
in the body list. Here is the shape:

@codeblock{
;; (hash-from-definitions Body ...) : Expr
}

Here is an example:

@racketblock[
(hash-from-definitions
  (define x 1)
  (define y (+ x x)))
(code:comment "expect #hash((x . 1) (y . 2))")
]

Based on this example alone, though, there are two main possibilities for what
we might want. The question is: How connected is the state of the hash to the
state of the variables?

@itemlist[
#:style 'ordered

@item{The hash is a @emph{snapshot} of the variables. If we produce a mutable
hash, then mutating the hash has no effect on the Racket variables. Remember, a
variable might continue to live after the hash is returned if it is captured by
a closure.}

@item{The hash @emph{shares state} with the variables. If we update the hash,
the variable value changes; if we @racket[set!] the variable, the hash's
corresponding entry changes.}

]

I want the second behavior.

To summarize the goal of @racket[hash-from-definitions], we want to
@emph{reinterpret} each variable definition in the body list and to likewise
reinterpret every reference and mutation of each variable in the body
sub-expressions. As with expressions, there is no grammar for body terms that
lets us perform case analysis and find the variable definitions; we need to use
@racket[local-expand] to get the body terms into a known grammar. Unlike
expressions, though, we cannot simply expand each body form fully; that would
violate Racket's @seclink["two-pass"]{two-pass expansion order}, causing
problems with forward references and mutually recursive definitions. Instead, we
must cooperate with Racket's two-pass expansion; specifically, we must mimic the
way it performs partial expansion in the first pass to uncover
definitions. There are two implementation strategies:

@itemlist[

@item{trampoline-style macro}
@item{the definition-context API}

]

The following sections demonstrate these two strategies. Both strategies perform
partial expansion using @racket[local-expand], but they use it differently.

@exercise[#:tag "reinterp2-distinguish"]{Create an example interaction that
distinguishes the two possibile behaviors of @racket[hash-from-definitions]
listed above. (One solution is shown in @secref["trampoline-body"].)}


@; ------------------------------------------------------------
@section[#:tag "trampoline-body"]{Processing Bodies with Trampoline-Style Macros}

One strategy to handling the body list is to let the Racket macro expander
manage the two-pass expansion but use a trampoline-style macro to pre-process
each body term during the first pass. Specifically, the trampoline-style macro
partially expands its argument and rewrites a variable definition to use the
hash instead by defining the name as an indirection macro instead. It returns
the new macro definition (or the partially expanded body term, in the other
cases), to the macro expander, which continues the first pass with the next
wrapped body term or continues to the second pass (expression expansion).

Here is the main @racket[hash-from-definitions] macro. It creates a mutable hash
and binds it to an auxiliary variable, and it wraps each argument body term with
the @racket[convert-defs] trampoline-style helper and puts it in a body term
context (specifically, the body of a @racket[let] expression).

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(hash-from-definitions Body ...) : Expr")
(define-syntax hash-from-definitions
  (syntax-parser
    [(_ body ...)
     #'(let ([h (make-hash)])
         (convert-defs body h) ...
         h)]))
]

The @racket[convert-defs] macro partially expands its body term argument. Since
it cooperating with the macro expander's handling of an internal definition
context (that is, body context), it passes @racket[(syntax-local-context)] as
the context argument to @racket[local-expand]. It passes @racket[#f] as the stop
list, which causes @racket[local-expand] to expand the root of the term until it
is no longer a macro application but not to recursively expand the term's
subexpressions. The result is a @shape{PartiallyExpandedBody}.

Unlike @shape{FullyExpandedExpr}, the @shape{PartiallyExpandedBody} shape does
not have a tidy grammar. A @shape{PartiallyExpandedBody} is defined by the
following property: if the term is a pair starting with an identifier, then the
identifier is not bound to a macro. In particular, if it starts with a core form
identifier, it is not necessarily a well-formed use of that core form, and the
client is responsible for validating the term.

Every macro that processes @shape{PartiallyExpandedBody} terms should handle
@racket[begin]. In this case, @racket[convert-defs] recurs trampoline-style to
the @racket[begin] form's subterms. Other than @racket[begin], the only core
form that @racket[convert-defs] specifically cares about is
@racket[define-values]. It handles variable definitions by creating auxiliary
variables to receive the results of the right-hand and then using the
@racket[define-key] helper macro to define the original ``variable'' names as
macros that access and update the hash. Here are the definitions of
@racket[convert-defs] and @racket[define-key]:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(convert-defs Body SimpleExpr[MutableHash]) : Body")
(define-syntax convert-defs
  (syntax-parser
    [(_ b:expr h)
     (define eb (local-expand #'b (syntax-local-context) #f))
     (syntax-parse eb
       #:literal-sets (kernel-literals)
       [(begin ~! b:expr ...)
        #'(begin (convert-defs b h) ...)]
       [(define-values ~! (x:id ...) rhs:expr)
        (with-syntax ([(tmp ...) (generate-temporaries #'(x ...))])
          #'(begin (define-values (tmp ...) rhs)
                   (define-key x tmp h) ...))]
       [eb
        #'eb])]))

(code:comment "(define-key x:Id Expr Expr[MutableHash]) : Body[{x}]")
(define-syntax define-key
  (syntax-parser
    [(_ x:id tmp:id h:expr)
     #'(begin (define-syntax x
                (make-variable-like-transformer
                  (quote-syntax (hash-ref h (Quote x)))
                  (quote-syntax (lambda (v) (hash-set! h (Quote x) v)))))
              (set! x tmp))]))
]

FIXME: need syntax-track-origin?

@examples[#:eval the-eval #:label #f
(define h
  (hash-from-definitions
    (define x 1)
    (define y (+ x x))
    (define (x-up!) (set! x (+ x y)))
    (define (xy) (* x y))))
(code:line (hash-ref h 'x)    (code:comment "expect 1"))
(code:line ((hash-ref h 'xy)) (code:comment "expect 2"))
((hash-ref h 'x-up!))
(code:line (hash-ref h 'x)    (code:comment "expect 3"))
(code:line ((hash-ref h 'xy)) (code:comment "expect 6"))
]

Problem:

@examples[#:eval the-eval #:label #f
(define-syntax define/get
  (syntax-parser
    [(_ var:id rhs:expr)
     #'(begin (define tmp rhs)
              (define (var) tmp))]))
(define h2
  (hash-from-definitions
    (define/get x 1)
    (define/get y (+ (x) (x)))))
(code:line ((hash-ref h2 'x)) (code:comment "expect 1 (??!)"))
(code:line ((hash-ref h2 'y)) (code:comment "expect 2"))
h2
]

We cannot represent @emph{all} variables by their symbolic names in the hash,
because the internal definition context may contain multiple variables with the
same symbolic name differing only in their scope sets (that is, the
representation of their lexical context). In the previous example, there were
two variables named @racket[tmp], but they were distinct variables because they
were introduced by different applications of the @racket[define/get] macro, so
they had different macro-introduction scopes. We need some sort of filter on
variables that prevents collisions: only selected variables will be represented
via the hash, and other variables will be left as ordinary variables.

@exercise[#:tag "reinterp2-original"]{Why would @racket[syntax-original?] be a
bad basis for deciding whether to include a variable in the result hash?}


@; ------------------------------------------------------------
@subsection[#:tag "trampoline-lctx"]{The Lexical Context Shape}

The @shape{LCtx} (lexical context) shape encompasses any term that is used
solely to represent a lexical context. That is, it is ultimately used as the
first argument to @racket[datum->syntax]. A common convention is to use an
identifier with the name @racket[HERE] for a @shape{LCtx} term.

Note that a @shape{LCtx} term does not represent a specific, fixed set of scopes
in Racket's scope-set hygiene model. During the macro expansion of a program, a
particular subterm will generally have environment scopes added to it as the
expander discovers binding forms around it, and it will have macro-introduction
scopes toggled as it is passed to macro transformers and returned in their
results. That is, the scopes associated with a lexical context vary during
expansion. The purpose of a @shape{LCtx} term is to track the changes in scopes
undergone by a lexical context.

As an example, we can use a @shape{LCtx} auxiliary argument to fix the problem
with @racket[hash-from-definitions]. We'll amend the behavior to the following:
only variables that have the same lexical context as the original
@racket[hash-from-definitions] expression are represented using the hash.

We amend @racket[hash-from-definitions] to create a @shape{LCtx} term
representing its own lexical context and pass it to the @racket[convert-defs]
helper:

@examples[#:eval the-eval #:no-result
(code:comment "(hash-from-definitions Body ...) : Expr")
(define-syntax hash-from-definitions
  (syntax-parser
    [(_ e:expr ...)
     #:with HERE (datum->syntax this-syntax 'HERE)
     #'(let ([h (make-hash)])
         (convert-defs e h HERE) ...
         (#%expression h))]))
]

The @racket[convert-defs] macro, likewise, passes the @shape{LCtx} argument
(called @racket[lctx]) to the @racket[define-key-maybe], which uses it to decide
whether to treat the given variable as a hash key or as an ordinary variable.

@examples[#:eval the-eval #:no-result
(code:comment "(convert-defs Body SimpleExpr[MutableHash] LCtx) : Body")
(define-syntax convert-defs
  (syntax-parser
    [(_ e:expr h lctx)
     (define ee (local-expand #'e (syntax-local-context) #f))
     (syntax-parse ee
       #:literal-sets (kernel-literals)
       [(define-values ~! (x:id ...) rhs:expr)
        #:with (tmp ...) (generate-temporaries #'(x ...))
        #'(begin (define-values (tmp ...) rhs)
                 (define-key-maybe x tmp h lctx) ...)]
       [(begin ~! e:expr ...)
        #'(begin (convert-defs e h lctx) ...)]
       [ee
        #'ee])]))
]

Finally, @racket[define-key-maybe] determines how to bind a variable. If the
variable has the same lexical context as the original
@racket[hash-from-definitions] expression, the variable is represented in the
hash and initialized to the temporary variable's value. Otherwise, it is defined
as an alias (that is, an alternate name) for the temporary variable using
@racket[make-rename-transformer]. To check whether the variable has the same
lexical context as @racket[lctx], we check whether they currently have the same
scope sets, and we do that by transferring the lexical context to the variable's
symbol and checking whether the two identifiers are @racket[bound-identifier=?].

@examples[#:eval the-eval #:no-result
(code:comment "(define-key-maybe x:Id Id SimpleExpr[MutableHash] LCtx) : Body[{x}]")
(define-syntax define-key-maybe
  (syntax-parser
    [(_ x:id tmp:id h:expr lctx)
     (cond [(bound-identifier=? #'x (datum->syntax #'lctx (syntax-e #'x)))
            #'(define-key x tmp h)]
           [else
            #'(define-syntax x (make-rename-transformer (quote-syntax tmp)))])]))
]

The @racket[define-key] helper is the same as before.

The problematic example from the last section now works as we would like:

@examples[#:eval the-eval #:label #f
(define-syntax define/get
  (syntax-parser
    [(_ var:id rhs:expr)
     #'(begin (define tmp rhs)
              (define (var) tmp))]))
(define h2
  (hash-from-definitions
    (define/get x 1)
    (define/get y (+ (x) (x)))))
(code:line ((hash-ref h2 'x)) (code:comment "expect 1"))
(code:line ((hash-ref h2 'y)) (code:comment "expect 2"))
h2
]

@exercise[#:tag "reinterp2-infnames"]{When a @racket[lambda] expression occurs
immediately as the right-hand side of a binding form (like
@racket[define-values]) with a single variable name, that variable name is used
as the procedure's name for printing. So when @racket[convert-defs] rewrites
@racket[define-values] forms to use freshly generated temporary names, it
affects the names of procedures. That is why the names of the procedures in the
example above have numeric suffixes. Fix it, but be careful not to change the
scope that the right-hand side expressions are evaluated in, and be careful not
to break any multi-valued expressions that currently work.}

@exercise[#:tag "reinterp2-link"]{Extend @racket[hash-from-definitions] to
support linkage clauses consisting of a hash expression and an import
declaration. Here is an example:

@racketblock[
(define h0 (make-hash))
(hash-set! h0 'x 1)
(define h1
  (hash-from-definitions
    #:link h0 #:import (x)
    (define y (+ x x))))
(hash-ref h1 'y) (code:comment "expect 2")
(eq? h0 h1)      (code:comment "expect #f")
]

@; Solution should include shape def!
}


@; ------------------------------------------------------------
@section[#:tag "defctx-api"]{Processing Bodies with the Definition-Context API}











@; ============================================================

@;{
Example: hash-from-definitions

v1: every definition has key
    - trampoline style
exercise: add syntax parameter `this-hash`
exercise: add #:base, #:import
v2: only declared (key _) variables are entered into table
    - collect loop, defctx!
}

@; FIXME: "global reinterpretation" => languages!

@(close-eval the-eval)
