;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require "styles.rkt"
          (for-label racket/base racket/match))

@title[#:tag "htdm" #:version ""]{How to Design Macros}

This guide is an attempt to adapt the ideas of
@italic{@hyperlink["https://htdp.org/"]{How to Design Programs} (HtDP)} to
designing Racket's macros and languages. The central idea of HtDP is the
``design recipe''; the kernel of the design recipe consists of the following
four steps:

@itemlist[#:style 'numbered

@item{Write a specification.}
@item{Write examples that can be turned into tests.}
@item{Choose an implementation strategy.}
@item{Finish the implementation and check it.}

]

@italic{HtDP} instantiates this kernel to teach the foundations of
programming. Its specification language is a semiformal language of types
including set-based reasoning and parametric polymorphism. Its implementation
strategies include structural recursion and case analysis following data type
definitions. It instantiates the implementation language to a series of simple
Scheme-like functional programming languages, and it provides a testing
framework.

Along the way, @italic{HtDP} fills in the design recipe's skeleton with idioms,
tricks, preferences, and limitations of Scheme-like (and ML-like)
mostly-functional programming languages. For example, it demonstrates
abstraction via parametric polymorphism and higher-order functions. To name some
of the limitations: it uses lexical scoping; it avoids reflection (eg, no
accessing structure fields by strings); it avoids @racket[eval]; it treats
closures as opaque; it (usually) avoids mutation; and so on. These parts of the
programming mental model tend to become invisible, until you compare with a
language that makes different choices.

This guide instantiates the design recipe kernel as follows: It introduces a
specification language called @emph{shapes}, combining features of grammars,
patterns, and types. The implementation strategies are more specialized, but
they are still informed by the shape of the macro inputs. The implementation
language is Racket with @racketmodname[syntax/parse] and some other standard
syntax libraries. Along the way, I cover some of the idioms and limitations of
the programming model for macros: macros (usually) respects lexical scoping;
they must respect the ``phase'' separation between compile time and run time;
they avoid @racket[eval]; they (usually) treats expressions as opaque; and so
on.

@; FIXME: transition

@; FIXME: "and languages"?
