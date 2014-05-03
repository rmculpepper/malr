#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base racket/match syntax/parse))

@(define the-eval (make-base-eval))

@title[#:tag "part-intro-validate"]{Intermediate Macrology: Specifying
and Validating Syntax}

@; ============================================================
@section[#:tag "valid-first"]{Basic Syntax Validation}

Let's revisit the @racket[andlet1] macro from
@secref["basic-binding-forms"]. The definition we gave then was

@racketblock+eval[#:eval the-eval
(define-syntax-rule (andlet1 var e1 e2)
  (let ([var e1])
    (if var e2 #f)))
]

The problem with this definition is that the macro does not
@emph{validate} it's syntax; it does not check that @racket[var] is an
identifier.

@interaction[#:eval the-eval
(andlet1 "not an id" #t #t)
]

Writing macros that validate their syntax requires something more
powerful than @racket[define-syntax-rule]. There are a couple
different options, but for now we'll skip straight to the most
advanced one: @racket[syntax-parse].

The definition of @racket[andlet1] using @racket[syntax-parse] looks
like this:

@racketblock+eval[#:eval the-eval
(require (for-syntax racket/base syntax/parse))

(define-syntax andlet1
  (lambda (stx)
    (syntax-parse stx
      [(_ var e1 e2)
       #:declare var identifier
       #'(let ([var e1])
           (if var e2 #f))])))
]

This definition reveals a few new issues that I'll mention briefly
now; we'll discuss them in more detail @later{later in this guide}.

@itemlist[

@item{We must import @racketmodname[racket/base] and
@racketmodname[syntax/parse] using @racket[for-syntax]. This allows us
to use those libraries in @emph{compile-time} code.}

@item{A macro is defined via @racket[define-syntax] with a right-hand
side that is a compile-time function---note the @racket[lambda] in the
definition above. The function implements the translation of terms
representing uses of the macro to their expanded forms.}

@item{Unlike @racket[define-syntax-rule] and @racket[syntax-rules]
macros, in @racket[syntax-parse] a syntax template is explicitly
marked with @litchar{#'}. The term @racket[#'@#,svar[term]] is just
reader notation for @racket[(@#,racket[syntax] @#,svar[term])].}

@item{The pattern starts with @racket[_] instead of the macro name
@racket[andlet1].}

]

Aside from these differences, the essense of the macro---the pattern
and template---is the same as the @racket[define-syntax-rule]
version. But we've added an @emph{annotation} to the @racket[var]
pattern variable via the @racket[#:declare] keyword, that constrains
@racket[var] to match only terms accepted by the @racket[identifier]
@emph{syntax class}. If the macro is used with a @racket[var] argument
that is not an identifier, the macro raises a syntax error, and
@racket[syntax-parse] uses the syntax class annotation to construct a
good error message.

@interaction[#:eval the-eval
(andlet1 "not an id" #t #t)
]

The definition above can be more compactly written by taking advantage
of the function-like @racket[define-syntax] location and the ``colon''
notation for syntax class annotations, which replaces the
@racket[#:declare] clause:

@racketblock+eval[#:eval the-eval
(define-syntax (andlet1 stx)
  (syntax-parse stx
    [(_ var:id e1 e2)
     #'(let ([var e1])
         (if var e2 #f))]))
]

Note that though the pattern contains @racket[var:id], the name of the
pattern variable is just @racket[var], and that's what we must use in
the template.

Why don't we annotate @racket[e1] and @racket[e2] to check that they
are expressions? There is in fact a syntax class named @racket[expr],
but it doesn't actually @emph{check} that a term is an expression. It
is impossible in general to check whether a term is a valid expression
without doing macro expansion, and we can't invoke the macro expander
until the static context of the term is known, and the static context
is determined by how it is used in the macro template. We can't tell a
definition from an expression, for example. Rather, the @racket[expr]
syntax class merely checks that the term is not a keyword, such as
@racket[#:declare]. Keywords are not self-quoting in Racket, so they
are not valid expressions, and we frequently want to distinguish
keywords from expressions when parsing syntax.

In short, we can annotate @racket[e1] and @racket[e2] with the
@racket[expr] syntax class, but we should keep in mind the syntactic
check is very shallow; we do it primarily to signal our intent to use
@racket[e1] and @racket[e2] as expressions.

@racketblock+eval[#:eval the-eval
(define-syntax (andlet1 stx)
  (syntax-parse stx
    [(_ var:id e1:expr e2:expr)
     #'(let ([var e1])
         (if var e2 #f))]))
]

@exercise{Add syntax validation to @racket[iflet] from
@exercise-ref["iflet"] by rewriting it to use @racket[syntax-parse].}

@exercise{Add syntax validation to @racket[my-let] from
@exercise-ref["my-let"] by rewriting it to use
@racket[syntax-parse]. Revisit each example misuse you
discovered. Which of the misuses are now rejected due to syntax
validation? Which are not?

@;{All but ``duplicate argument name'' are now caught.}
}


@; ============================================================
@(close-eval the-eval)
