;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string racket/struct-info syntax/transformer
                     racket/contract rackunit))

@(define the-eval (make-malr-eval))
@(the-eval '(require (for-syntax racket/match)
                     racket/match racket/string))

@; ============================================================
@title[#:tag "compile-time"]{Compile-Time Computation and Information}

The section discusses macros that do computation at compile time, and it
introduces a shape for compile-time information bound to an identifier.


@; ------------------------------------------------------------
@section[#:tag "ct-computation"]{Compile-Time Computation}

@margin-note{This example is based on the @racketmodname[scramble/regexp]
library. The @racketmodname[parser-tools/lex] library implements a similar
notation.}

I hate writing regular expressions. At least, I hate writing them once they get
over twenty characters long, or have more than two ``report'' groups, or have
character classes involving special characters, or....

Let's design a macro that takes a pleasant, compositional S-expression notation
and automatically translates it at compile time to a regular expression literal
--- specifically, a @racket[pregexp] literal.


@subsection[#:tag "whenever-computation"]{Computation, Whenever}

Wait! Why make this a macro? I can define an ordinary run-time AST datatype
(call it @type{RE}) for representing regular expressions, and I can write a
function that translates an @type{RE} to a @racket[pregexp] string.

Here is the @type{RE} type. For simplicity, it only represents handles a subset
of @secref["regexp-syntax" #:doc '(lib "scribblings/reference/reference.scrbl")].

@codeblock{
;; An RE is one of
;; - (re:or (NonemptyListof RE))
;; - (re:cat (Listof RE))
;; - (re:repeat Boolean RE)
;; - (re:report RE)
;; - (re:chars (NonemptyListof Range))
;; A Range is (rng Char Char)
}
@examples[#:eval the-eval #:no-result
(struct re:or (res) #:prefab)
(struct re:cat (res) #:prefab)
(struct re:repeat (plus? re) #:prefab)
(struct re:report (re) #:prefab)
(struct re:chars (ranges) #:prefab)
(struct rng (lo hi) #:prefab)
]
I'll explain why I use @racket[#:prefab] structs in a later section.

Here is the code to translate an @type{RE} value into a
@racket[pregexp]-compatible string. The functions are organized according to
what nonterminal in the @nonterm{regexp} grammar they produce, to handle the
precedence of regular expression syntax. For example, producing an
@nonterm{atom} from a concatentation @type{RE} requires wrapping its
@nonterm{regexp} form with @litchar{(?:}@racket[HOLE]@litchar{)}.

@examples[#:eval the-eval #:no-result
(code:comment "emit-regexp : RE -> String")
(define (emit-regexp re)
  (match re
    [(re:or res) (string-join (map emit-pces res) "|")]
    [_ (emit-pces re #f)]))
(code:comment "emit-pces : RE [Boolean] -> String")
(define (emit-pces re [rec? #t])
  (match re
    [(re:cat res) (string-join (map emit-pces res) "")]
    [_ (emit-pce re rec?)]))
(code:comment "emit-pce : RE [Boolean] -> String")
(define (emit-pce re [rec? #t])
  (match re
    [(re:repeat #f re) (format "~a*" (emit-atom re))]
    [(re:repeat #t re) (format "~a+" (emit-atom re))]
    [_ (emit-atom re rec?)]))
(code:comment "emit-atom : RE [Boolean] -> String")
(define (emit-atom re [rec? #t])
  (match re
    [(re:report re) (format "(~a)" (emit-regexp re))]
    [(re:chars ranges) (format "[~a]" (string-join (map emit-range ranges) ""))]
    [_ (cond [rec? (format "(?:~a)" (emit-regexp re))]
             [else (error 'emit-regexp "bad RE: ~e" re)])]))
(code:comment "emit-range : Range -> String")
(define (emit-range r)
  (match r
    [(rng c c) (emit-char c)]
    [(rng lo hi) (format "~a-~a" (emit-char lo) (emit-char hi))]))
(code:comment "emit-char : Char -> String")
(define (emit-char c)
  (define (special? c) (for/or ([sp (in-string "()*+?[]{}.^\\|")]) (eqv? c sp)))
  (if (special? c) (string #\\ c) (string c)))
]

Here is an example:
@examples[#:eval the-eval #:label #f
(emit-regexp
 (re:repeat #f
  (re:cat (list (re:report (re:repeat #t (re:chars (list (rng #\a #\z)))))
                (re:repeat #t (re:chars (list (rng #\space #\space))))))))
]

So, the ergonomics leave a bit to be desired. It would be possible to improve
the interface by using friendlier functions instead of the raw AST constructors,
of course. Or we could even define an S-expression notation and parse it into
the @type{RE} type using @racket[match]. All of potentially incurs additional
run-time overhead, and there is also the overhead of @racket[pregexp] call
itself.

In any case, this code represents a complete, self-contained unit of
functionality. Let's wrap up the code above as module:

@racketblock[
(module re-ast racket/base
  (require racket/match racket/string)
  (provide (struct-out re:or)
           (struct-out re:cat)
           ELIDED
           emit-regexp)
  ELIDED)
]

We can leave a friendlier front end as a task for a separate module.

@; FIXME! Avoid copying!
@examples[#:eval the-eval #:hidden
(module re-ast racket/base
  (require racket/match racket/string)
  (provide (all-defined-out))

  (struct re:or (res) #:prefab)
  (struct re:cat (res) #:prefab)
  (struct re:repeat (plus? re) #:prefab)
  (struct re:report (re) #:prefab)
  (struct re:chars (ranges) #:prefab)
  (struct rng (lo hi) #:prefab)

  (define (emit-regexp re)
    (match re
      [(re:or res) (string-join (map emit-pces res) "|")]
      [_ (emit-pces re #f)]))
  (define (emit-pces re [rec? #t])
    (match re
      [(re:cat res) (string-join (map emit-pces res) "")]
      [_ (emit-pce re rec?)]))
  (define (emit-pce re [rec? #t])
    (match re
      [(re:repeat #f re) (format "~a*" (emit-atom re))]
      [(re:repeat #t re) (format "~a+" (emit-atom re))]
      [_ (emit-atom re rec?)]))
  (define (emit-atom re [rec? #t])
    (match re
      [(re:report re) (format "(~a)" (emit-regexp re))]
      [(re:chars ranges) (format "[~a]" (string-join (map emit-range ranges) ""))]
      [_ (cond [rec? (format "(?:~a)" (emit-regexp re))]
               [else (error 'emit-regexp "bad RE: ~e" re)])]))
  (define (emit-range r)
    (match r
      [(rng c c) (emit-char c)]
      [(rng lo hi) (format "~a-~a" (emit-char lo) (emit-char hi))]))
  (define (emit-char c)
    (define (special? c) (for/or ([sp (in-string "()*+?[]{}.^\\|")]) (eqv? c sp)))
    (if (special? c) (string #\\ c) (string c))))
]

@; ------------------------------------------------------------
@subsection[#:tag "macro-front-end"]{A Macro Front-End}

Let's add a macro ``front end'' to the @type{RE} type and @racket[emit-regexp]
function. Specifically, the macro should support a friendlier notation that it
parses into a compile-time @type{RE} value, translates to a string, and converts
to a @racket[pregexp] literal, all at compile time.

Here is a shape for representing (a subset of) regular expressions:

@codeblock{
;; RE ::= (or RE ...+)
;;      | (cat RE ...)
;;      | (* RE)
;;      | (+ RE)
;;      | (report RE)
;;      | (chars CharRange ...+)
;; CharRange ::= Char | [Char Char]
}

I have called the shape @shape{RE}, the same as the @type{RE} type. In fact, the
interpretation of the @shape{RE} term is a compile-time @type{RE} value.  
We can import the @type{RE} type and @racket[emit-regexp] function into the
@emph{compile-time environment} as follows:

@examples[#:eval the-eval #:no-result
(require (for-syntax 're-ast))
]

The @racket[re] syntax class, then, should have a single attribute whose value
is an @type{RE} value.

Before we define the syntax class, we should decide how to recognize the
literals in the shape definition (aka @emph{grammar}) above. In
@secref["quasiquote"], I said there are two options: symbolic literals and
reference literals. In this case, I want to use names that are already defined
by Racket, but their interpretations here have nothing to do with their Racket
meanings. More importantly, I don't plan to support macro-like extensions to
this syntax, which is one major reason to recognize literals by reference
instead of symbolically. So let's recognize the @shape{RE} literals
symbolically. We can do that by declaring them with @racket[#:datum-literals]
instead of @racket[#:literals]. Here are the syntax class definitions:

@examples[#:eval the-eval #:no-result
(begin-for-syntax
  (define-syntax-class char-range
    #:attributes (ast) (code:comment "Range")
    (pattern c:char
             #:attr ast (let ([c (syntax->datum #'c)]) (rng c c)))
    (pattern [lo:char hi:char]
             #:attr ast (rng (syntax->datum #'lo) (syntax->datum #'hi))))
  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    (pattern (or e:re ...+)
             #:attr ast (re:or (datum (e.ast ...))))
    (pattern (cat e:re ...)
             #:attr ast (re:cat (datum (e.ast ...))))
    (pattern (* e:re)
             #:attr ast (re:repeat #f (datum e.ast)))
    (pattern (+ e:re)
             #:attr ast (re:repeat #t (datum e.ast)))
    (pattern (report e:re)
             #:attr ast (re:report (datum e.ast)))
    (pattern (chars r:char-range ...+)
             #:attr ast (re:chars (datum (r.ast ...))))))
]

The @racket[my-px] macro simply calls @racket[emit-regexp] on the parsed
@type{RE} value, then calls @racket[pregexp] to convert that to a (compile-time)
regular expression value.

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(code:comment "(my-px RE) : Expr")
(define-syntax my-px
  (syntax-parser
    [(_ e:re)
     #`(Quote #,(pregexp (emit-regexp (datum e.ast))))]))
]

Note that we use @racket[quote] to wrap the value returned by @racket[pregexp].

Here is the example from the previous section translated to use the macro's
notation:

@examples[#:eval the-eval #:label #f
(my-px (* (cat (report (+ (chars [#\a #\z])))
               (+ (chars #\space)))))
]


@exercise[#:tag "static:re-string"]{Update the @shape{RE} shape with the
following case:
@codeblock{
;; RE ::= ... | String
}
You can use @racket[string] as a syntax class annotation to recognizes string
terms.

A string is interpreted as the concatenation of the singleton character sets of
each character in the string. For example:
@racketblock[
(my-px (* "ab"))
(code:comment "expect #px\"(?:ab)*\"")
]}


@; ------------------------------------------------------------
@section[#:tag "static-shape"]{The Static Shape}

The @shape{Static[T]} shape is a parameterized shape that recognizes identifiers
that refer to compile-time information of type @type{T}. The interpretation of a
@shape{Static[T]} reference is the compile-time @type{T} value.

The corresponding @racket[static] syntax class is parameterized by a predicate
and a description. The syntax class matches an identifier if the identifier is
bound in the @emph{normal environment} to a @emph{compile-time} value accepted
by the predicate; the @racket[value] attribute contains the compile-time value.

That is, the @shape{Static} shape contains identifiers bound with
@racket[define-syntax], @racket[let-syntax], and so on. I'll call this a
@emph{static binding}, as opposed to a @emph{variable bindings} created by
@racket[define], @racket[let], and so on. Static bindings are also created by
macros such as @racket[struct]: the name of a struct type carries compile-time
information about the struct type, including references to its predicate and
accessor functions. This information is used by macros like @racket[match] to
implement pattern matching; it is also used by @racket[struct-out] to get the
names to export. (And remember, a @emph{static} binding in the @emph{normal
environment} is different from a @emph{variable} binding in the
@emph{compile-time environment}, even though both refer to compile-time values.)

@;{
That means, by the way, that a Racket macro name is ``simply'' an identifier
that belongs to @shape{Static[Syntax -> Syntax]}. Well, almost.
}

@margin-note{Terminology: I don't like ``bound as syntax''. I'm not totally
happy with ``bound statically'' either, though. ``bound to compile-time
information'' is too verbose. ``bound at compile time'' is wrong. Alternatives?}

Let's extend our little regular expression language with the ability to define
and use @shape{RE} names.

Here is the shape of the definition form:
@margin-note{Notation: @shape{~} or @shape{::} or ??}
@codeblock{
;; (define-re x:Id RE) : Body[{x ~ RE}]
}

I'm using the notation from @secref["shapes-types-scopes"] to indicate the
bindings introduced by a @shape{Body} term, but I have extended it with the
notation @shape{x ~ RE} to mean that @shape{x} is bound @emph{statically} to a
compile-time value of type @type{RE} --- as opposed to @shape{x : T}, which
means that @shape{x} is bound as a variable to a run-time value of type
@type{T}.

The variants of the @type{RE} type are represented by prefab structs, which are
@racket[read]able and --- more importantly --- @racket[quote]able. So we can
implement the definition macro as follows:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(define-syntax define-re
  (syntax-parser
    [(_ name:id e:re)
     #`(define-syntax name (quote #,(datum e.ast)))]))
]

That is, @racket[define-re] parses the @shape{RE} term to get a compile-time
@type{RE} value. But it cannot directly create a static binding for
@racket[name]; it must do so by expanding to a @racket[define-syntax] term. So
the macro must convert the compile-time @type{RE} value that it has now into an
expression that will produce an equivalent value later, when the macro expander
processes the @racket[define-syntax] form. Since the value is @racket[read]able,
we can do that with @racket[quote]. (If the value were not readable, then this
would create ``3D syntax'', and modules using the macro would fail to
compile. Or more precisely, compilation would succeed but the compiler would be
unable to serialize the compiled code to a @tt{.zo} file.)

To allow references to static @type{RE} bindings, we extend the @shape{RE} shape
as follows:

@codeblock{
;; RE ::= ... | Static[RE]
}

The @racket[re] syntax class needs a new pattern for references to @shape{RE}
names, and that pattern needs a helper predicate to recognize @type{RE}
values. The existing patterns are unchanged.

@racketblock[
(begin-for-syntax
  (code:comment "re-ast? : Any -> Boolean")
  (code:comment "Shallow check for RE AST constructor. Sufficient?")
  (define (re-ast? v)
    (or (re:or? v) (re:cat? v) (re:repeat? v) (re:report? v) (re:chars? v)))

  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    ELIDED
    (pattern (~var name (static re-ast? "name bound to RE"))
             #:attr ast (datum name.value))))
]

@examples[#:eval the-eval #:hidden #:escape UNQUOTE
;; --- Real definitions ---
(begin-for-syntax
  (code:comment "re-ast? : Any -> Boolean")
  (code:comment "Shallow check for RE AST constructor. Sufficient?")
  (define (re-ast? v)
    (or (re:or? v) (re:cat? v) (re:repeat? v) (re:report? v) (re:chars? v)))
  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    (pattern (or e:re ...+)
             #:attr ast (re:or (datum (e.ast ...))))
    (pattern (cat e:re ...)
             #:attr ast (re:cat (datum (e.ast ...))))
    (pattern (* e:re)
             #:attr ast (re:repeat #f (datum e.ast)))
    (pattern (+ e:re)
             #:attr ast (re:repeat #t (datum e.ast)))
    (pattern (report e:re)
             #:attr ast (re:report (datum e.ast)))
    (pattern (chars r:char-range ...+)
             #:attr ast (re:chars (datum (r.ast ...))))
    (pattern (~var name (static re-ast? "name bound to RE"))
             #:attr ast (datum name.value))))
;; --- Redefine to get new syntax class defn ---
(define-syntax my-px
  (syntax-parser
    [(_ e:re)
     #`(Quote #,(pregexp (emit-regexp (datum e.ast))))]))
]

Now we can define intermediate @shape{RE} names and compose them into more
complicated regular expressions:

@examples[#:eval the-eval #:label #f
(define-re word (+ (chars [#\a #\z])))
(define-re spacing (+ (chars #\space)))
(my-px (* (cat (report word) spacing)))
]

If we attempt to refer to a name that is not defined as a @shape{RE}, then we
get an appropriate error:
@examples[#:eval the-eval #:label #f
(eval:error (my-px (cat word list)))
]

We can inspect the compile-time value bound to an @shape{RE} name by using
@racket[syntax-local-value], which is is the low-level mechanism underneath
@racket[static]:

@examples[#:eval the-eval #:label #f
(phase1-eval (syntax-local-value (quote-syntax word)))
]

@; ------------------------------------------------------------
@section[#:tag "multi-inferfaces"]{Static Information with Multiple Interfaces}

There are still a few issues:
@itemlist[#:style 'ordered

@item{It would be nice if we could also use @shape{RE} names like @racket[word]
and @racket[spacing] as variables.}

@item{The shallow @racket[re-ast?] test doesn't guarantee that the name was
defined using @racket[define-re]. After all, anyone can create a prefab struct
named @racket[re:repeat].}

@item{This design does not allow forward references: an @shape{RE} name must be
defined before it is used. But it is often preferable to define complex objects
in a top-down order.}

]
We can fix the first two issues by adding a generative (that is, not prefab)
struct wrapper around the @type{RE} value, and making it support the procedure
interface so it acts as an identifier macro. The next section shows how to
support forward references, at least in most contexts.

The macro expander considers any name that is statically bound to a procedure to
be a macro. It invokes the macro's transformer on uses of the macro both in
operator position and as a solitary identifier. A macro that allows being used
as a solitary identifier is called an @emph{identifier macro}. (If the macro's
value is a @racket[set!-transformer], it is also invoked when the macro is used
as the target of a @racket[set!] expression.)

In Racket, any non-prefab struct can act as a procedure by implementing the
@emph{procedure interface}, represented by the @racket[prop:procedure] struct
type property. The macro system defines other interfaces, such as
@racket[prop:rename-transformer] and @racket[prop:set!-transformer], and macros
can also define their own interfaces. For example, the @racket[struct] form
defines an interface, @racket[prop:struct-info], for representing compile-time
information about struct types; the @racket[match] macro defines an interface,
@racket[prop:match-expander], for implementing new @racket[match] pattern forms;
and so on. Thus one name can carry multiple kinds of static information and
behavior by being bound to a struct type that implements multiple interfaces.

We could even define and export our own interface for values representing
@shape{RE} names. But that would conflict with our goal of restricting @type{RE}
names to those defined through our @racket[define-re] macro, which enforces
invariants on the values carried by @shape{RE} names. Furthermore, it commits us
to a representation; if we change how we represent information (as we will in
the next section), it can break code that relies on the public interface. These
problems can be mitigated with well-formedness checks and adapters, but that is
additional effort and complexity, and it often doesn't completely fix the
problem. In this example, the costs and risks don't seem worth the (absent)
benefits. Another possibility is to define an interface but keep it private,
only using it within a library. That avoids the problems above. It still doesn't
seem useful in this particular example, though.

So, we will define a new struct type that implements the procedure interface so
@shape{RE} names can be used as expressions, but our macros will recognize the
struct type specifically, without going through an additional interface
indirection. Here is the definition:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (code:comment "A RE-Binding is (re-binding RE (Syntax -> Syntax))")
  (struct re-binding (ast transformer)
    #:property prop:procedure (struct-field-index transformer)))
]

When used as a procedure, an @racket[re-binding] instance just forwards the call
to its @racket[transformer] field, so when @racket[define-re] constructs an
@racket[re-binding] instance, it must provide a suitable transformer
function. We can use the @racket[make-variable-like-transformer] library
function to construct an identifier macro that always produces the same
expansion. Here is the updated @racket[define-re]:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
;; --- Note, wrong re binding at this point, must redefine later! ---
(require (for-syntax syntax/transformer))
(define-syntax define-re
  (syntax-parser
    [(_ name:id e:re)
     #`(define-syntax name
         (re-binding (Quote #,(datum e.ast))
                     (make-variable-like-transformer
                      (quote-syntax (my-px name)))))]))
]

Now we must update @racket[re] to extract the @type{RE} AST value in the
@shape{RE}-name case. Again, the other variants remain unchanged:

@racketblock[
(begin-for-syntax
  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    ELIDED
    (pattern (~var name (static re-binding? "name bound to RE"))
             #:attr ast (re-binding-ast (datum name.value)))))
]

@examples[#:eval the-eval #:hidden #:escape UNQUOTE
;; ---- Redefine ----
(require (for-syntax syntax/transformer))
(begin-for-syntax
  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    (pattern (or e:re ...+)
             #:attr ast (re:or (datum (e.ast ...))))
    (pattern (cat e:re ...)
             #:attr ast (re:cat (datum (e.ast ...))))
    (pattern (* e:re)
             #:attr ast (re:repeat #f (datum e.ast)))
    (pattern (+ e:re)
             #:attr ast (re:repeat #t (datum e.ast)))
    (pattern (report e:re)
             #:attr ast (re:report (datum e.ast)))
    (pattern (chars r:char-range ...+)
             #:attr ast (re:chars (datum (r.ast ...))))
    (pattern (~var name (static re-binding? "name bound to RE"))
             #:attr ast (re-binding-ast (datum name.value)))))
(define-syntax define-re
  (syntax-parser
    [(_ name:id e:re)
     #`(define-syntax name
         (re-binding (Quote #,(datum e.ast))
                     (make-variable-like-transformer
                      (quote-syntax (my-px name)))))]))
(define-syntax my-px
  (syntax-parser
    [(_ e:re)
     #`(Quote #,(pregexp (emit-regexp (datum e.ast))))]))
]

Now the following example works; we can use @racket[word] and @racket[spacing]
like variables:

@examples[#:eval the-eval #:label #f
(define-re word (+ (chars [#\a #\z])))
(define-re spacing (+ (chars #\space)))
(define-re word+spacing (cat (report word) spacing))
(list word spacing (my-px (* word+spacing)))
]

We can also verify that the compile-time information stored by an @shape{RE}
name is no longer a prefab struct; it is an opaque wrapper which prints as a
procedure:

@examples[#:eval the-eval #:label #f
(phase1-eval (syntax-local-value (quote-syntax word)))
]


@exercise[#:tag "static:print"]{Update the definition of @racket[re-binding] so
that instances of the struct print as @racket["#<RE>"]. You can do this by
implementing the @racket[prop:custom-write] interface.

@racketblock[
(phase1-eval (format "~s" (syntax-local-eval (quote-syntax word))))
(code:comment "expect \"#<RE>\"")
]}

@exercise[#:tag "static:match" #:stars 1]{Update the definition of
@racket[re-binding] so that it also acts as a match pattern name. As a match
pattern, it takes some number of pattern arguments; these are the patterns used
to match the regular expression's @racket[report] results. That is, an
@shape{RE} name used as a match pattern expands like this:

@racketblock[
(word+spacing pat)
==>
(pregexp word+spacing (list _ pat))
]

See @racket[match] for an explanation of the syntax of match patterns; see
@racket[prop:match-expander] for an explanation of the interface.}

@exercise[#:tag "static:match2" #:stars 2]{Update your solution to
@exercise-ref["static:match"] to check that when used as a match pattern, the
@shape{RE} name receives the correct number of arguments. That is, the first
example below should succeed (because @racket[word+spacing] has one
@racket[report]), but the others should cause an error:
@racketblock[
(word+spacing the-word)         (code:comment "okay")
(word+spacing the-word extra)   (code:comment "error: wrong number of patterns, expected 1")
(spacing the-spaces)            (code:comment "error: wrong number of patterns, expected 0")
]}


@; ------------------------------------------------------------
@section[#:tag "two-pass"]{Two-Pass Expansion}

To support forward references requires knowing a little about how Racket
processes definitions.

The Racket macro expander processes definition contexts (module bodies,
@racket[lambda] bodies, and so on) in two passes. The first pass discovers
definitions; the second pass expands (remaining) expressions.

In the first pass, Racket expands each body term until it reaches a core form,
but it does not recur into the core form's sub-expressions. Once it reaches a
core form, it does a shallow case analysis. If the form is
@racket[define-values], it marks the names as variables in the local
environment. If the form is @racket[define-syntaxes], it evaluates the
right-hand side as a compile-time expression and binds the names statically. If
the form is @racket[begin], it flattens it away and recurs on the contents. (So
a macro can expand into multiple definitions by grouping them with
@racket[begin].) Otherwise the form is an expression form, and it leaves that
until the next pass. After the case analysis, it continues to the next body term.

In the second pass, the expander knows all of the names bound in the recursive
definition context, both variable names and static names. The expander then
processes the remaining expressions: the sub-expressions of core expression
forms and the right-hand sides of @racket[define-values] definitions.

@; ----------------------------------------
@subsection[#:tag "ct-fwd-ref"]{Forward References}

How can we support forward references for compile-time data?

Since static definitions are evaluated in order, the static information for a
@shape{RE} name cannot just contain an @type{RE} AST value. It must contain a
thunk or promise or something similar that allows us to get the AST value on
demand. Let's use a promise. Here is the updated struct definition:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(require (for-syntax racket/promise))
(begin-for-syntax
  (code:comment "A RE-Binding is (re-binding (Promise RE) (Syntax -> Syntax))")
  (struct re-binding (astp transformer)
    #:property prop:procedure (struct-field-index transformer)))
]

Now @racket[define-re] cannot eagerly parse the @shape{RE} term; instead, it
must create a promise that parses it later. Here's one implementation:

@examples[#:eval the-eval #:no-result #:escape UNQUOTE
(begin-for-syntax
  (code:comment "parse-re-from-def : Syntax -> RE")
  (code:comment "Receives the entire `define-re` term.")
  (define (parse-re-from-def stx)
    (syntax-parse stx
      [(_ _ e:re) (datum e.ast)])))

(define-syntax define-re
  (syntax-parser
    [(_ name:id _)
     #`(begin
         (define-syntax name
           (re-binding (delay (parse-re-from-def (quote-syntax #,this-syntax)))
                       (make-variable-like-transformer
                        (quote-syntax (my-px name)))))
         (void (my-px name)))]))
]

Within a @racket[syntax-parser] clause, @racket[this-syntax] is bound to the
syntax object currently being parsed. In this case, that's the syntax of the
@racket[define-re] use. The reason for passing the whole definition syntax to
@racket[parse-re-from-def] instead of just the @shape{RE} term is that by
default @racket[syntax-parse] reports syntax errors using the leading identifier
of its argument as the ``complaining party''. (This behavior can be overridden
with the @racket[#:context] argument, though.)

Why does the expansion include @racket[(void (my-px name))]? That expression
ensures that the promise eventually gets forced. Since it occurs within an
expression, it is delayed until pass two, when all forward references should
have been defined. If we left it out, then a syntactically invalid @shape{RE}
definition would be accepted as long as it was never used.

Finally, we must update @racket[re] to force the promise from a @shape{RE}
name. Here is a basic implementation:
@racketblock[
(begin-for-syntax
  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    ELIDED
    (pattern (~var name (static re-binding? "name bound to RE"))
             #:attr ast (force (re-binding-astp (datum name.value))))))
]

One flaw in this implementation is that if it is given a recursive @shape{RE}
definition, it produces an internal error about re-entrant promises. Here is a
version that uses a parameter to detect that situation and signals a better
error:

@racketblock[
(begin-for-syntax
  (code:comment "running : (Parameter (Promise RE))")
  (code:comment "Currently running RE promises, used to detect cycles.")
  (define running (make-parameter null))

  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    ELIDED
    (pattern (~var name (static re-binding? "name bound to RE"))
             #:attr ast (let ([p (re-binding-ast (datum name.value))])
                          (when (member p (running))
                            (raise-syntax-error #f "recursive RE" #'name))
                          (parameterize ((running (cons p (running))))
                            (force p))))))
]


@examples[#:eval the-eval #:hidden #:escape UNQUOTE
;; ---- Update ----
(require (for-syntax racket/promise syntax/transformer))

(begin-for-syntax
  ;; A RE-Binding is (re-binding (Promise RE) (Syntax -> Syntax))
  (struct re-binding (ast transformer)
    #:property prop:procedure (struct-field-index transformer))

  (define running (make-parameter null))

  (define-syntax-class re
    #:attributes (ast) (code:comment "RE")
    #:datum-literals (or cat * + report chars)
    (pattern (or e:re ...+)
             #:attr ast (re:or (datum (e.ast ...))))
    (pattern (cat e:re ...)
             #:attr ast (re:cat (datum (e.ast ...))))
    (pattern (* e:re)
             #:attr ast (re:repeat #f (datum e.ast)))
    (pattern (+ e:re)
             #:attr ast (re:repeat #t (datum e.ast)))
    (pattern (report e:re)
             #:attr ast (re:report (datum e.ast)))
    (pattern (chars r:char-range ...+)
             #:attr ast (re:chars (datum (r.ast ...))))
    (pattern (~var name (static re-binding? "name bound to RE"))
             #:attr ast (let ([p (re-binding-ast (datum name.value))])
                          (when (member p (running))
                            (raise-syntax-error #f "recursive RE" #'name))
                          (parameterize ((running (cons p (running))))
                            (force p))))
    #;
    (pattern (~var name (static re-binding? "name bound to RE"))
             #:attr ast (let ([p (re-binding-ast (datum name.value))])
                          (cond [(member p (running)) #f]
                                [else (parameterize ((running (cons p (running))))
                                        (force p))]))
             #:fail-when (if (datum ast) #f #'name) "recursive RE")

    ))

(begin-for-syntax
  (define (parse-re-from-def stx)
    (syntax-parse stx
      [(_ _ e:re) (datum e.ast)])))

(define-syntax define-re
  (syntax-parser
    [(_ name:id _)
     #`(begin
         (define-syntax name
           (re-binding (delay (parse-re-from-def (quote-syntax #,this-syntax)))
                       (make-variable-like-transformer
                        (quote-syntax (my-px name)))))
         (void (my-px name)))]))

(define-syntax my-px
  (syntax-parser
    [(_ e:re)
     #`(Quote #,(pregexp (emit-regexp (datum e.ast))))]))
]

With those changes, forward references work, at least within modules and within
internal definition contexts like @racket[let] bodies:

@examples[#:eval the-eval #:label #f
(let ()
  (define-re para (* (cat word spacing)))
  (define-re word (+ (chars [#\a #\z])))
  (printf "word = ~s\n" word)
  (define-re spacing (+ (chars #\space)))
  para)
]

We get reasonable messages for the following error cases:

@examples[#:eval the-eval #:label #f
(eval:error
(let ()
  (define-re uses-undef (cat (chars #\a) undef))
  'whatever))
(eval:error
(let ()
  (define-re rec (cat (chars #\a) rec))
  'whatever))
]


There are other ways we could manage the delayed resolution of @shape{RE}
names. For example, we could extend the AST type with a new variant for names,
eagerly parse most of the AST and create promises only for instances of the name
variant. One benefit of that approach is that most @shape{RE} syntax errors
could be caught when the definition is processed instead of when the promise is
forced. Some drawbacks are that it requires adding a new function to traverse
the AST forcing the name nodes, and it involves either changing the @type{RE}
type or creating a substantially similar @type{RE-With-Promises} type.


@; ----------------------------------------
@subsection[#:tag "two-pass-scoping"]{The Peculiarities of Scoping in Two-Pass Expansion}

The two-pass expansion and its treatment of macros means that definition
contexts in Racket are not purely recursive; they also have a slight sequential
aspect. Consider the behavior of the following example:
@examples[#:eval the-eval #:label #f
(let ()
  (define-syntax m
    (syntax-parser
      [(_ e:expr) #'(printf "outer ~s\n" e)]))
  (let ()
    (m (begin (m 123) 456))
    (define-syntax m
      (syntax-parser
        [(_ e:expr) #'(printf "inner ~s\n" e)]))
    (m 789)))
]

Simple recursive scoping would predict that all three references to @racket[m]
in the inner @racket[let] body refer to the inner definition of @racket[m]. But
the first use of @racket[m] is head-expanded before the inner definition of
@racket[m] is discovered, so it refers to the outer definition. It's argument,
though, is not expanded until pass two, so it refers to the inner @racket[m], as
does the third use of @racket[m]. The third use of @racket[m] is expanded before
the second, though.

What if we simply delete the outer macro definition?
@examples[#:eval the-eval #:label #f
(let ()
  (let ()
    (m (begin (m 123) 456))
    (define-syntax m
      (syntax-parser
        [(_ e:expr) #'(printf "inner ~s\n" e)]))
    (m 789)))
]

Now in pass one the expander assumes that the first use of @racket[m] is a
function application, and @racket[m] is a variable that might be defined
later. So it saves the whole expression for pass two. Then in pass two it
realizes that the expression is not a function application but a macro
application, and it expands the macro. This is good, right? It's what one would
expect given a macro definition in a recursive scope.

But there are limits. Consider the following example, where the macro produces a
definition instead of an expression:
@examples[#:eval the-eval #:label #f
(eval:error
(let ()
  (m a)
  (define-syntax m
    (syntax-parser
      [(_ x:id) #'(define x 1)]))
  (m b)
  (+ a b)))
]
As in the previous example, the macro expansion initially classifies the first
use of @racket[m] as a function application. In the second pass, though, when it
expands the macro, it expands it in a strict expression context. That is because
it is unwilling to make further changes to the environment in the second pass;
it is frozen at the end of the first pass.

The greatest scoping peculiarities of definition contexts arise from macro names
that are shadowed in the middle of an inner scope.

@lesson{Don't shadow macro names.}

Unfortunately, many names in Racket that seem like variable names are actually
implemented as macro bindings. One example is functions with keyword arguments,
to reduce run-time overhead for keyword checking and default arguments. Another
example is bindings exported with @racket[contract-out], to compute the negative
blame party from the use site.

@lesson{As much as possible, avoid shadowing entirely.}



@(close-eval the-eval)
