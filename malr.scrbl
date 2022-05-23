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
- breaking hygiene (hash-view)
- compile-time API
  - write transformers, not macro-generating-macros
  - set!-transformers (define-box-variable, define-parameter-variable)
- ids bound to static info (match-hash-view)
  - first with separate names
  - then combine names (one struct, two interfaces)
- local-expand
  - test for quoted value
  - assert macro that prints (selected) free variable values!
- "polylithic" macros
  - Idea: syntax-parameter for error reporting?

Racket changes:
- `body`, etc syntax classes
- #:check / ~check, more flexible error reporting

}

@; @include-section["old/old.scrbl"]


@(cc-footer)
