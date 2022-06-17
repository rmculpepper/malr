;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require "styles.rkt")

@title[#:style 'toc #:version malr-version]{Macros and Languages in Racket}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(local-table-of-contents #|#:style 'immediate-only|#)

@include-section["part-intro.scrbl"]
@include-section["part-shapes.scrbl"]
@include-section["part-basic.scrbl"]
@include-section["part-compound.scrbl"]
@include-section["part-defshape.scrbl"]
@include-section["part-enum.scrbl"]
@include-section["part-multi.scrbl"]
@include-section["part-recursive.scrbl"]
@include-section["part-static.scrbl"]
@include-section["part-unhygienic.scrbl"]
@include-section["part-reinterpret.scrbl"]
@include-section["part-reinterpret2.scrbl"]

@;{

Add to existing sections:
- Exercises
  - generalize cond to take "body list" (intro terminology,
    "body term" vs "body list" ??)
  - and+ (with #:do clauses)
  - (my-match-list Expr (Id ...) Expr) : Expr
  - exercise recap
    - If you didn't follow the design recipe, you missed the point! Fail!

Next sections:
- compile-time API
  - write transformers, not macro-generating-macros
- identifier macros, set!-transformers
  - define-box-variable, define-parameter-variable
  - (let-lazy ([x:Id Expr] ...) Expr{x...}) -- use promise, identifier macros
- local-expand
  - test for quoted value
  - assert macro that prints (selected) free variable values!
- "polylithic" macros
  - Idea: syntax-parameter for error reporting?

- hash-from-definitions - produces (mutable?) hash
  - v1: include all definitions as keys
    - problem: identifier => symbol is not injective
  - v2: only include symbols declared "public"
  - v3: (alt) only include symbols with same lexical scope...
  - v4: add base, import keys

- updated, mini version of Advanced Macrology & Typed Scheme ?


Chapter 13: Pitfalls and Gotchas
- generate quote-syntax, not #' -- analogy to (printf unknown-string)

Chapter 2:
- "body list"
  - "module body list", "local body list" / "internal body list"
- "body list with final expression" is eh, instead use ???

Racket changes:
- `body`, etc syntax classes
- #:check / ~check, more flexible error reporting
- validate PartiallyExpandedBody, etc
  - partial-local-expand/check : Syntax ExpandContext Stops DefCtx -> Syntax
  - check-partially-expanded-syntax : Syntax ExpandContext -> Void
- syntax-track-origin w/o macro name
- generic pass1 support
- metafunction in core?
- lock syntax to given interpretation

}

@; @include-section["old/old.scrbl"]


@(cc-footer)
