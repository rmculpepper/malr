#lang scribble/manual
@(require "styles.rkt")

@title[#:style 'toc]{Macros and Languages in Racket}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(local-table-of-contents #|#:style 'immediate-only|#)

@include-section["part-basic.scrbl"]

@; ============================================================
@section{Topics}

@; ------------------------------------------------------------
@bold{Preliminaries}

You should also know some set theory. Just because.

@; ------------------------------------------------------------
@bold{Basic macrology}

A macro is a rewrite rule. It only applies to certain terms (no
general rewriters like optimization). It only applies in certain
context. Expansion is outward-in: discovery process.

Expressions have dynamic and static contexts.

Dynamic context: continuation marks, parameters, other state (order of
evaluation, etc).

Static context: environment, syntax parameters. More exotic: phase,
submodule.

Basic hygiene.

Basic pattern matching.

Racket language elements: expressions, definitions, module-form forms,
etc. Modules and languages.

Syntax ergonomics, eg positional forms vs keyords vs identifier-tagged
subforms.

Don't use a macro when a function would suffice.

Not every macro needs to be written.

Error checking (with @racket[syntax-parse] and without).

@; ------------------------------------------------------------
@bold{Intermediate macrology}

The structure of syntax. Think in terms of nonterminals, one function
per nonterminal.

Identifier comparisons: bound-id=?, free-id=? (and phases).

Communication via syntax-local-value.

Communication via compile-time state (eg, identifier tables). Define
vs attach.

Compile-time helper functions, modules, begin-for-syntax, phases.

3d syntax.

Hygiene details: marks, renames, namespaces.

Breaking hygiene: how to do it right, and alternatives.

Rule: Never use @racket[syntax->datum] on an expression or a term
containing expressions.

Below the pattern-matching abstractions: @racket[syntax-e],
@racket[datum->syntax], and @racket[quote-syntax].

How to test macros: Test run-time behavior, test binding, test syntax
errors.

Source information.

Syntax properties.

The Why of Phases.

Monolithic vs microlithic macros. Trampoline style macros.

@; ------------------------------------------------------------
@bold{Advanced macrology}

Head expansion.

Full expansion, analysis, and transformation (instrumentation). In
macro w/ local-expand; in run-time tool w/ expand.

(refer back to monolithic/microlithic/trampoline)
