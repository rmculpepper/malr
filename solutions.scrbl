#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base racket/match))

@(define the-eval (make-base-eval))

@title[#:tag "solutions" #:style '(toc)]{Solutions to Selected Exercises}

@(local-table-of-contents)

@;{Each solution gets a separate section and a separate page, to
prevent spoilers.}

@;@section[#:tag "solutions-basic"]{Solutions to Exercises in @secref["basic"]}

@solution-section[#:tag "noisy-v1"]

@racketblock[
(define-syntax-rule (noisy-v1 expr)
  (begin (printf "evaluating ~s\n" 'expr) expr))
]

@solution-section[#:tag "noisy-v2"]

@racketblock[
(define-syntax-rule (noisy-v2 expr)
  (begin (printf "evaluating ~s..." 'expr) 
         (begin0 expr (printf "done\n"))))
]


@(close-eval the-eval)
