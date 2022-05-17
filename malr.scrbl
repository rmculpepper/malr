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

@;{

Add to existing sections:
- keep macros simple: use run-time helpers
- SimpleExpr shape ?
- Exercises
  - generalize cond to take "body list" (intro terminology,
    "body term" vs "body list" ??)
  - and+ (with #:do clauses)
  - (my-match-list Expr (Id ...) Expr) : Expr
  - exercise recap
    - If you didn't follow the design recipe, you missed the point! Fail!

Next sections:
- Datum shape, quasiquote
  - quote / quote-syntax on prefab structs!
  - quasiquote example?
- Static shape (use simple version of regexp example?)

- breaking hygiene (hash-view)
- compile-time API
  - write transformers, not macro-generating-macros
  - set!-transformers (define-box-variable, define-parameter-variable)
- ids bound to static info (match-hash-view)
  - first with separate names
  - then combine names (one struct, two interfaces)
- regexp DSL
  - monolithic compiler-like macro: parse -> codegen
  - multiple modules
  - more complicated phases
  - two-pass expansion (why can't we have forward references?)

- "polylithic" macros
  - Idea: syntax-parameter for error reporting?

Racket changes:
- `body`, etc syntax classes


}




@; @include-section["old/old.scrbl"]


@(cc-footer)
