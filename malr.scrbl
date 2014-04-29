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

Misc advice:
@itemlist[
@item{Don't use a macro when a function would suffice.}
@item{Not every macro needs to be written.}
]


@; ------------------------------------------------------------
@bold{Intermediate Macrology: syntax-parse and error-checking1}

syntax-parse and error-checking

The structure of syntax. Think in terms of nonterminals, one function
per nonterminal.

syntax ergonomics, eg positional forms vs keyords vs identifier-tagged
subforms.

Ergonomics/conventions: Eval subexprs in original order. Do error
checking of arguments in order, when possible.

special subforms w/o extensibility

syntax-case and error-checking (as aside)

Error checking without @racket[syntax-parse]


@; ------------------------------------------------------------
@bold{Intermediate Macrology: phases}

Compile-time helper functions, modules, begin-for-syntax, phases.

The Why of Phases.

syntax templates, phases


@; ------------------------------------------------------------
@bold{Intermediate Macrology: under the hood}

Below the pattern-matching abstractions: @racket[syntax-e],
@racket[datum->syntax], and @racket[quote-syntax].

Rule: Never use @racket[syntax->datum] on an expression or a term
containing expressions.

Source information.

Syntax properties.


@; ------------------------------------------------------------
@bold{Intermediate Macrology: communication w/ syntax-local-value}

Communication via syntax-local-value.
 - ordinary communication
 - extensible subforms


@; ------------------------------------------------------------
@bold{Intermediate Macrology: misc}

Racket language elements: expressions, definitions, module-form forms,
etc. Modules and languages.

Communication via compile-time state (eg, identifier tables). Define
vs attach.

#%expression

Identifier comparisons: bound-id=?, free-id=? (and phases).

3d syntax.

Hygiene details: marks, renames, namespaces, gradual discovery.

Breaking hygiene: how to do it right, and alternatives.

How to test macros: Test run-time behavior, test binding, test syntax
errors.

Monolithic vs microlithic macros. Trampoline style macros.


@; ------------------------------------------------------------
@bold{Advanced Macrology}

Head expansion.

Full expansion, analysis, and transformation (instrumentation). In
macro w/ local-expand; in run-time tool w/ expand.

(refer back to monolithic/microlithic/trampoline)
