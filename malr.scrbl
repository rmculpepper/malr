#lang scribble/manual
@(require "styles.rkt")

@title[#:style 'toc #:version ""]{Macros and Languages in Racket}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(local-table-of-contents #|#:style 'immediate-only|#)

@include-section["part-prelim.scrbl"]
@include-section["part-basic.scrbl"]
@include-section["part-intro-validate.scrbl"]
@include-section["part-communication.scrbl"]
@include-section["solutions.scrbl"]

#|
Chapter 2: Procedural Macros
- What is a macro (revisited?)
  - a name bound via define-syntax (etc) to a compile-time procedure
- need phase separation
- how to use phases (briefly)
- syntax-parse and syntax/#' (by example)
2.0 Preamble: What is a macro?
2.1 Redo existing macros w/ basic syntax-parse, #', syntax-e
2.2 Basic syntax api: with-syntax, syntax->list, raise-syntax-error, syntax->datum
2.3 Syntax APIs: source locations
  - consuming (eg assert, test-case, etc)
  - produce/propagate, syntax/loc -- note: unreliable
...
2.4 Phases in more detail: require for-syntax (syntax/stx?), begin-for-syntax
2.5 (skippable) Low-level macro writing (cf Fear of Macros?)

Chapter 3: Using syntax-parse
- stxclass annotation for error reporting
- mix in context-sensitive checks
- stxclasses and attributes: macro helper computation
- backtracking
- hit major features: head patterns, ....

Chapter 4: Macros that Communicate
- syntax-local-value
  - prop:procedure, new props = interfaces
- compile-time state
  - and the phase reset, idempotency

Macros as grammar extensions (or somewhere earlier?)

Chapter 5: Extensible Extensions
- via syntax-local-value
- via local-expand
  - integrity protection: tainting/dye-packs

|#


@; ============================================================
@section[#:tag "todo-topics"]{More Topics to Cover}

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
@bold{Intermediate Macrology: Syntax Objects}

Below the pattern-matching abstractions: @racket[syntax-e],
@racket[datum->syntax], and @racket[quote-syntax].

Rule: Never use @racket[syntax->datum] on an expression or a term
containing expressions.

Source information.

Syntax properties.


@; ------------------------------------------------------------
@bold{Intermediate Macrology: Phases}

Compile-time helper functions, modules, begin-for-syntax, phases,
require for-{syntax,template}.

The Why of Phases.

syntax templates, phases, env catalog


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

template tricks: @racket[template], ellipsis-escaping, etc

macro-defining macros, using @racket[make-X-transformer] to prevent
code explosion


@; ------------------------------------------------------------
@bold{Advanced Macrology}

Head expansion.

Full expansion, analysis, and transformation (instrumentation). In
macro w/ local-expand; in run-time tool w/ expand.

(refer back to monolithic/microlithic/trampoline)

@; ------------------------------------------------------------
@bold{Languages}

Two ways of embedding language X in Racket:

@itemlist[#:style 'ordered
@item{@racketblock[(Racket) expr ::= .... ALT X]
      X forms are Racket macros. X grammar must not collide with Racket grammar.}
@item{@racketblock[(Racket) expr ::= .... ALT (begin-X X)]
      X forms are not Racket macros. X grammar can collide with Racket
      grammar. Can have separate @racket[begin-X] forms for different
      interpretations/behaviors of X fragments (eg for regular
      expressions: match vs enumerate).}
]

In (1), if @racket[X ::= .... ALT expr] also, then it's not a language,
it's probably just a library or a data type.

(1) works most naturally with compositional translations. (2) can
handle non-compositional translation, but can also do compositional
via trampoline style.

Can also have hybrid style (3?) where every X form, when used in
Racket context, implicitly does @racket[begin-X] with self inside.

Other languages notes:

@racket[#%module-begin]

namespace mangagement: subtracting Racket bindings, renaming, etc

custom macro expanders, @racket[syntax-local-value] vs @racket[static]
