;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string racket/struct-info syntax/transformer racket/syntax
                     racket/contract racket/list rackunit syntax/parse/experimental/template))

@(define the-eval (make-malr-eval))
@(the-eval '(require racket/match racket/string (for-syntax racket/list)))

@; ============================================================
@title[#:tag "reinterpret-body"]{Reinterpreting Body Terms}

@; FIXME: "global reinterpretation" => languages!

@; ------------------------------------------------------------
@section[#:tag "partial-expand"]{Partial Expansion}

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
