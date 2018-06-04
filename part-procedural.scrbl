#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base racket/match syntax/parse))

@(define the-eval (make-base-eval))

@title[#:tag "procedural"]{Procedural Macros}

@; ============================================================
@section[#:tag "what-is-a-macro"]{What is a Macro?}

A macro is a name bound by @racket[define-syntax] (or
@racket[let-syntax], etc) to a @emph{compile-time procedure}.

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
|#

@; ============================================================
@(close-eval the-eval)
