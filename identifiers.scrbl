#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base))

@(define the-eval (make-base-eval))

@title{Beyond Expressions}

@section{Binding}

Previously we saw how to use a macro can control the dynamic context
of an expression. We can use a macro to control what appears in the
scope of an expression. The macro @racket[let1] will bind the value of
@racket[e] to @racket[x] within @racket[body].

Previously we saw how a macro can place an expression in a new dynamic
context. We can also use a macro to place an expression in a new
@emph{static context}---that is, under binding forms such as
@racket[lambda].

@racketblock[#:eval the-eval
(define-syntax-rule (let1 x e body)
  ((lambda (x) body)
   e))
]