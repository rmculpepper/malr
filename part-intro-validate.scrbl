#lang scribble/manual
@(require scribble/eval
          "styles.rkt"
          (for-label racket/base racket/match syntax/parse))

@(define the-eval (make-base-eval))

@title[#:tag "part-intro-validate"]{Specifying and Validating Syntax}

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
checking is very shallow; we do it primarily to signal our intent to
use @racket[e1] and @racket[e2] as expressions.

@racketblock+eval[#:eval the-eval
(define-syntax (andlet1 stx)
  (syntax-parse stx
    [(_ var:id e1:expr e2:expr)
     #'(let ([var e1])
         (if var e2 #f))]))
]

@exercise{Add syntax validation to @racket[iflet] from
@exercise-ref["iflet"] by rewriting it to use @racket[syntax-parse].}

@exercise[#:tag "my-let-valid"]{Add syntax validation to
@racket[my-let] from @exercise-ref["my-let"] by rewriting it to use
@racket[syntax-parse]. Revisit each example misuse you
discovered. Which of the misuses are now rejected due to syntax
validation? Which are not?

@;{All but ``duplicate argument name'' are now caught.}
}


@; ============================================================
@section[#:tag "valid-cs"]{Context-Sensitive Syntax Checking}

In @exercise-ref["my-let-valid"] you rewrote @racket[my-let] using
@racket[syntax-parse] and added syntax class annotations to validate
that the variable arguments are identifiers. Here's the code:

@racketblock+eval[#:eval the-eval
(define-syntax (my-let stx)
  (syntax-parse stx
    [(_ ([var:id rhs:expr] ...) body:expr)
     #'((lambda (var ...) body) rhs ...)]))
]

You should have also tested your solution against the four kinds of
misuses that the @racket[define-syntax-rules] version didn't
catch. Let's try them again, and see which ones are caught by the new
version of @racket[my-let].

@interaction[#:eval the-eval
(code:line (my-let ([1 2]) 'body) (code:comment "was `lambda: not an identifier, ...'"))
(code:line (my-let ([a 1] [a 2]) 'body) (code:comment "was `lambda: duplicate argument name'"))
(code:line (my-let ([#:a 1] [b 2]) 'body) (code:comment "was `arity mismatch'"))
(code:line (my-let ([[a 1] 2]) 'body) (code:comment "previously ran without error"))
]

Three of the four misuses now signal an error in terms of
@racket[my-let]. Let's look more closely at the one that doesn't.

@interaction[#:eval the-eval
(code:line (my-let ([a 1] [a 2]) 'body) (code:comment "was `lambda: duplicate argument name'"))
]

Neither occurrence of @racket[a] is wrong by itself; only the use of
both of them in the same sequence of bindings is problematic. In other
words, each binding variable is subject to a @emph{context-sensitive}
constraint---it must be distinct from any previous binding variable in
the sequence. Syntax class annotations represent @emph{context-free}
constraints: here, that the term must be an identifier.

We can check context-sensitive constraints explicitly by inserting
code between the pattern and the template.

@racketblock+eval[#:eval the-eval
(define-syntax (my-let stx)
  (syntax-parse stx
    [(_ ([var:id rhs:expr] ...) body:expr)
     (let loop ([vars (syntax->list #'(var ...))]
                [seens null])
       ;; vars is list of variables to check
       ;; seens is prefix of variables already seen
       (when (pair? vars)
         (when (for/or ([seen (in-list seens)])
                 (bound-identifier=? (car vars) seen))
           (raise-syntax-error #f
             "duplicate identifier"
             stx (car vars)))
         (loop (cdr vars) (cons (car vars) seens))))
     #'((lambda (var ...) body) rhs ...)]))
]

This code contains some new features. Again, I'll mention them briefly
here, and they will be explained in more detail @later{later}.

@itemlist[

@item{We use @racket[(syntax->list #'(var ...))] to get a list of the
@racket[var] identifiers.}

@item{We use @racket[bound-identifier=?] to check whether the current
@racket[var] is equal to a previous @racket[var] in the list.}

@item{When a duplicate is discovered, we call
@racket[raise-syntax-error], which takes @racket[#f] (nearly always;
see the documentation for details), an error message, a ``big term'',
and a ``little term''. The big term is the whole expression; its
leading identifier will be used as the complaining party---thus,
``@racket[my-let]: duplicate identifier''. The little term is the
precise location of the error---here, it is the duplicate variable.}

]

With the new error-checking code, @racket[my-let] catches the
duplicate instead of passing it along to @racket[lambda] to discover:

@interaction[#:eval the-eval
(code:line (my-let ([a 1] [a 2]) 'body) (code:comment "was `lambda: duplicate argument name'"))
]

I've written out the error-checking code above to give you some
insight into how it is done, but there is a shorter way to write
it. Racket has a function called @racket[check-duplicate-identifier]
that finds duplicate identifiers using
@racket[bound-identifier=?]. And @racket[syntax-parse] offers a
@racket[#:fail-when] clause that replaces the call to
@racket[raise-syntax-error]. Here is the simpler version of the macro:

@racketblock+eval[#:eval the-eval
(define-syntax (my-let stx)
  (syntax-parse stx
    [(_ ([var:id rhs:expr] ...) body:expr)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(var ...)))
                 "duplicate identifier"
     #'((lambda (var ...) body) rhs ...)]))
]

Recall that a syntax error contains a big term and a little term;
@racket[syntax-parse] knows the big term, and
@racket[check-duplicate-identifier] returns either @racket[#f] or a
duplicate identifier---the identifier is the little term.

@lesson{When writing a binding form, use @racket[bound-identifier=?]
or @racket[check-duplicate-identifier] to check for collisions between
binders.}

@exercise[#:tag "my-let*-distinct"]{Write a macro
@racket[my-let*-distinct] that behaves like @racket[let*] except that
it requires its variables to be distinct, like @racket[let]. Hint:
rename the previous definition of @racket[my-let*] from
@secref["basic-rec"] and reuse it as a helper macro.}




@;{
;; exercise: minimatch w/ unique vars!
}


@; ============================================================

@;{
Terminology

terms; terms can be specialized by kind of data (identifier, keyword,
etc)

forms; forms are matter of intent/interpretation and context used in
(expression, binder/reference, binding-clause, etc)

maybe call compound terms w/ interpretation a "clause" of some sort
(eg, cond clause, binding clause (in let), etc). (???)
}


@; ============================================================
@(close-eval the-eval)
