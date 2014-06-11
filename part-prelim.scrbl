#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base racket/match))

@title[#:tag "prelim" #:version "" #:style '(unnumbered)]{Preliminaries}

This guide assumes that you have a basic working knowledge of Racket
and functional programming. @other-doc['(lib
"scribblings/guide/guide.scrbl")] is sufficient for the former, and
@hyperlink["http://www.ccs.neu.edu/home/matthias/HtDP2e/"]{How to
Design Programs, 2nd edition} is good for the latter.

Most topics have have exercises that reinforce and expand on their
lessons. Try to do every exercise. When an exercise has a linked
solution page, use it to check your solution; the solution pages
sometimes have important comments that aren't repeated in the main
text.

Difficult exercises are marked with one or more stars (@STAR); they
may require extra thought and care. It is okay to skip these and
revisit them later, but sometimes I use an exercise in one section as
the basis of a worked example in a later section, so you may encounter
solution ``spoilers.''
