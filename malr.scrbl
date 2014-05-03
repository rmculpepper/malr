#lang scribble/manual
@(require "styles.rkt")

@title[#:style 'toc]{Macros and Languages in Racket}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(local-table-of-contents #|#:style 'immediate-only|#)

@include-section["part-basic.scrbl"]
@include-section["part-intro-validate.scrbl"]

@; ============================================================
@section{More Topics to Cover}


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
@bold{Intermediate Macrology: Specifying and Validating Syntax}

@racket[syntax-parse], syntax specification (grammars), validation and
error-checking

The structure of syntax. Think in terms of nonterminals. When writing
functions on syntax, write one function per nonterminal.

Do functions on subterms first, then talk about moving code to
stxclass attrs.

syntax ergonomics, eg positional forms vs keyords vs identifier-tagged
subforms.

Ergonomics/conventions: Eval subexprs in original order. Do error
checking of arguments in order, when possible.

special subforms (w/o extensibility)

@racket[syntax-case] and error-checking (aside)

Note: this section might need forward references to Phases section
(just point out compile-time vs run-time code, say Phases deals with
in more detail later).

@; ------------------------------------------------------------
@bold{Intermediate Macrology: Phases}

Compile-time helper functions, modules, begin-for-syntax, phases,
require for-{syntax,template}.

The Why of Phases.

syntax templates, phases, env catalog


@; ------------------------------------------------------------
@bold{Intermediate Macrology: Under the Hood}

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
@bold{Intermediate Macrology: uncategorized}

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

Applicable structs and macros that act as expressions as well as
something else (eg struct names).


@; ------------------------------------------------------------
@bold{Advanced Macrology}

Head expansion.

Full expansion, analysis, and transformation (instrumentation). In
macro w/ local-expand; in run-time tool w/ expand.

(refer back to monolithic/microlithic/trampoline)
