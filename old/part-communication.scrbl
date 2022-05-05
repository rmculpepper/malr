#lang scribble/manual
@(require scribble/eval
          "../styles.rkt"
          (for-label racket/base racket/match syntax/parse))

@(define the-eval (make-base-eval))

@title[#:tag "communication"]{Macros That Communicate}

@; ============================================================
@section[#:tag "comm-static-info"]{Identifiers Bound to Static Information}

Here are a few examples of communication between macros:

@itemlist[

@item{A struct name (defined via @racket[struct] can be used in a
@racket[match] pattern to match instances of that struct type. A
struct name can also be used with the @racket[struct-out] provide
form.}

@item{Redex's @racket[reduction-relation] and
@racket[define-metafunction] forms require the name of a language
(defined via @racket[define-language]) so they can recognize
nonterminal symbols. Redex's @racket[term] form recognizes uses of
names defined as metafunctions and automatically applies them.}

@item{The @racket[unit] form and related macros determine the
variables they bind according to signature names (defined with
@racket[define-signature]).}

]

For the @racket[match] form to recognize and destructure instances of
a given struct type, it must have the struct type's predicate and
accessor functions---that is, the @racket[match] macro must have
references (identifiers) to those bindings. Of course, the
@racket[match] macro must also be able to tell whether a name is bound
as a struct name as opposed to, for example, an ordinary function
definition.
(The struct name carries not only static information but also acts as
an expression (as the constructor, in particular). Such names are
discussed in more detail in @secref["comm-multiple-meanings"].)

Similarly, @racket[reduction-relation] and @racket[unit] must be able
to recognize names bound as language names and signature names,
respectively, and then extract the compile-time information from them:
the set of nonterminal names and the signature contents, respectively.

All of the examples above use @emph{bound names} as the channel of
communication. This means that the normal tools for scoping and name
management apply: names carrying static information can be renamed
(using @racket[rename-out] or @racket[rename-in]), kept private, or
shadowed like any other binding.

A name is bound to static information using @racket[define-syntax],
like macros. Instead of a transformer procedure, however, the
right-hand side should produce a value of whatever type is chosen to
represent that kind of static information. Creating a new
(compile-time) struct type to represent a new kind of information is
the surest way to avoid ambiguity due to overlapping
representations. (A more flexible alternative is discussed in
@secref["comm-multiple-meanings"].)

Let us create a ``record'' system with the following communicating forms:

@defform[(define-record-type type-id [field-id ..])]{

Defines a new record type distinct from
any other record type (but not necessarily distinct from other Racket
types), with associated constructor, predicate, and accessor
functions.
}

@defform[(record-case value-expr clause ... maybe-else-clause)
         #:grammar ([clause [(record-type-id field-id ...) result-expr]]
                    [maybe-else-clause (code:line)
                                       [else result-expr]])]{

Destructures a record value, like a specialized version of Racket's
@racket[match].
}

@racketblock[
(provide (for-syntax (struct-out record-info))
         define-record-type
         record-case)

(begin-for-syntax
  (struct record-info (maker predicate accessor)))

(define-syntax define-record-type
  (lambda (stx)
    (syntax-parse stx
      [(_ type:id [field:id ...])
       #:fail-when (check-distinct-identifier (syntax->list #'(field ...)))
                   "duplicate field name"
       (with-syntax ([maker (format-id #'type "make-~a" #'type)]
                     [predicate (format-id #'type "~a?" #'type)]
                     [((accessor accessor-index) ...)
                      (for/list ([field-id (syntax->list #'(field ...))]
                                 [index (in-naturals 1)])
                        (list (format-id #'type "~a-~a" #'type field-id)
                              index))])
         #'(begin
             (define record-id (gensym))
             (define (maker field ...) (vector record-id field ...))
             (define (predicate x)
               (and (vector? x)
                    (= (vector-length x) (add1 (length '(field ...))))
                    (eq? (vector-ref x 0) record-id)))
             (define (accessor x)
               (unless (predicate x)
                 (error 'accessor "expected a ~s record, got: ~e" 'type x))
               (vector-ref x accessor-index))
             (define-syntax type
               (make-record-info (quote-syntax maker)
                                 (quote-syntax predicate)
                                 (list (quote-syntax accessor) ...)))))])))
]


@; NOTE: quote-syntax, good habit


@; Exercise: define-view for minimatch
@; Exercise: support structs in minimatch (read docs)

@; ============================================================

@section[#:tag "comm-multiple-meanings"]{Identifiers With Multiple Meanings}
@section[#:tag "comm-attach"]{Define vs Attach}

@; ============================================================
@(close-eval the-eval)
