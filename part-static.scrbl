;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang scribble/manual
@(require (except-in scribble/eval examples)
          (only-in scribble/example examples)
          (only-in scribble/bnf nonterm)
          "styles.rkt"
          (for-label racket/base syntax/parse syntax/datum racket/match syntax/macro-testing
                     racket/string rackunit))

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

Here is the @type{RE} type. For simplicity, it only handles a subset of the
features of @secref["regexp-syntax" #:doc '(lib
"scribblings/reference/reference.scrbl")].

@codeblock{
;; An RE is one of
;; - (re:or (Listof RE))
;; - (re:cat (Listof RE))
;; - (re:repeat Boolean RE)
;; - (re:report RE)
;; - (re:chars (Listof Range))
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
           ___
           emit-regexp)
  ___)
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
@section[#:tag "macro-front-end"]{A Macro Front-End}

Let's add a macro ``front end'' to the @type{RE} type and @racket[emit-regexp]
function. Specifically, the macro should support a friendlier notation that it
parses into a compile-time @type{RE} value, translates to a string, and converts
to a @racket[pregexp] literal, all at compile time.

We can simply import the @type{RE} type and @racket[emit-regexp] function into
the @emph{compile-time environment} as follows:
@examples[#:eval the-eval #:no-result
(require (for-syntax 're-ast))
]





@; ------------------------------------------------------------
@section[#:tag "static-shape"]{The Static Shape}

The @shape{Static[T]} shape is a parameterized shape that recognizes identifiers
that refer to compile-time information of type @type{T}. The interpretation of a
@shape{Static[T]} reference is the compile-time @type{T} value.

The corresponding @racket[static] syntax class .. FIXME

FIXME

Generally, rather than using instances of @shape{Static} all over your
specifications, you should give a name to the instance of @shape{Static} that
your code uses.

....


@(close-eval the-eval)
