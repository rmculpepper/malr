;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string racket/struct-info syntax/transformer racket/syntax
                     racket/promise
                     racket/contract racket/list rackunit syntax/parse/experimental/template))

@(define the-eval (make-malr-eval))
@(the-eval '(require racket/match racket/string racket/promise
                     (for-syntax racket/list syntax/transformer)))

@; ============================================================
@title[#:tag "reinterpret-body"]{Reinterpreting Body Terms}

@; FIXME: "global reinterpretation" => languages!

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
listed above. (A solution is shown in @secref["trampoline-body"].)}


@; ------------------------------------------------------------
@section[#:tag "trampoline-body"]{Processing Bodies with Trampoline-Style Macros}

FIXME: @shape{LCtx} shape?



@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(hash-from-definitions Body ...) : Expr")
(define-syntax hash-from-definitions
  (syntax-parser
    [(_ body ...)
     #'(let ([h (make-hash)])
         (convert-defs body h) ...
         h)]))
]

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(convert-defs Body SimpleExpr[MutableHash]) : Body")
(define-syntax convert-defs
  (syntax-parser
    [(_ e:expr h)
     (define ee (local-expand #'e (syntax-local-context) #f))
     (syntax-parse ee
       #:literal-sets (kernel-literals)
       [(define-values ~! (x:id ...) rhs:expr)
        (with-syntax ([(tmp ...) (generate-temporaries #'(x ...))])
          #'(begin (define-values (tmp ...) rhs)
                   (define-key x tmp h) ...))]
       [(begin ~! e:expr ...)
        #'(begin (convert-defs e h) ...)]
       [ee
        #'ee])]))

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

Solution:

@examples[#:eval the-eval #:no-result
(define-syntax hash-from-definitions
  (syntax-parser
    [(_ e:expr ...)
     #:with HERE (datum->syntax this-syntax 'HERE)
     #'(let ([h (make-hash)])
         (convert-defs e h HERE) ...
         (#%expression h))]))

(define-syntax convert-defs
  (syntax-parser
    [(_ e:expr h here)
     (define ee (local-expand #'e (syntax-local-context) #f))
     (syntax-parse ee
       #:literal-sets (kernel-literals)
       [(define-values ~! (x:id ...) rhs:expr)
        #:with (tmp ...) (generate-temporaries #'(x ...))
        #'(begin (define-values (tmp ...) rhs)
                 (define-key-maybe x tmp h here) ...)]
       [(begin ~! e:expr ...)
        #'(begin (convert-defs e h here) ...)]
       [ee
        #'ee])]))

(define-syntax define-key-maybe
  (syntax-parser
    [(_ x:id tmp:id h:expr here)
     (cond [(bound-identifier=? #'x (datum->syntax #'here (syntax-e #'x)))
            #'(begin (define-syntax x
                       (make-variable-like-transformer
                        (quote-syntax (hash-ref h (quote x)))
                        (quote-syntax (lambda (v) (hash-set! h (quote x) v)))))
                     (set! x tmp))]
           [else
            #'(define-syntax x (make-rename-transformer (quote-syntax tmp)))])]))
]

@examples[#:eval the-eval #:label #f
(define-syntax define/get
  (syntax-parser
    [(_ var:id rhs:expr)
     #'(begin (define tmp rhs)
              (define (var) tmp))]))
(define h
  (hash-from-definitions
    (define/get x 1)
    (define/get y (+ (x) (x)))))
(code:line ((hash-ref h 'x)) (code:comment "expect 1 (??!)"))
(code:line ((hash-ref h 'y)) (code:comment "expect 2"))
h
]




@exercise[#:tag "reinterp2-base"]{Extend @racket[hash-from-definitions] to
support an optional starting hash and import declaration. Here is an example:

@racketblock[
(define h0 (make-hash))
(hash-set! h0 'x 1)
(define h
  (hash-from-definitions
    #:base h0 #:import (x)
    (define y (+ x x))))
(hash-ref h 'y) (code:comment "expect 2")
(eq? h0 h)      (code:comment "expect #t")
]

@; Solution should include shape def!
}

@exercise[#:tag "reinterp2-original"]{Why would @racket[syntax-original?] be a
bad basis for deciding whether to include a variable in the result hash?}

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


@(close-eval the-eval)
