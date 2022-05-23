;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang at-exp racket/base
(require racket/runtime-path
         racket/date
         racket/format
         (for-syntax racket/base)
         (for-syntax syntax/parse))
(require scribble/basic
         ;; scribble/struct
         scribble/manual
         scribble/decode
         scribble/racket
         scribble/core
         scribble/base
         scribble/html-properties
         scribble/example
         (for-label racket/base))
(provide schemekw
         schemevar
         shape
         type
         declare-keyword
         ___
         ==>
         Quote
         Syntax
         ALT
         !
         STAR
         HOLE
         ELIDED
         expr_
         fail_
         datum_

         (rename-out [malr-title title]
                     [malr-section section]
                     [malr-subsection subsection])

         malr-version
         tech/reference
         tech/guide
         secref/reference
         secref/guide
         lesson
         lesson*
         later
         exercise
         exercise-number
         exercise-ref
         exercise-number-ref
         solution-section
         make-malr-eval
         cc-footer)

(define draft-mode? #t)

(define malr-version
  (let ([now (current-date)])
    (define (~02r n) (~r n #:min-width 2 #:pad-string "0"))
    (apply format "2-~a~a.~a" ;; (~02r (modulo (date-year now) 100))
           (map ~02r (list (date-month now) (date-day now) (date-hour now))))))

(define (malr-title #:style [style #f] #:tag [tag #f] #:version [vers malr-version] . pre-content)
  (list (apply title #:style style #:tag tag #:version vers pre-content)
        (tag-note tag)))

(define (malr-section #:tag [tag #f] . pre-content)
  (list (apply section #:tag tag pre-content)
        (tag-note tag)))

(define (malr-subsection #:tag [tag #f] . pre-content)
  (list (apply subsection #:tag tag pre-content)
        (tag-note tag)))

(define-syntax-rule (schemekw x) (schemekeywordfont (symbol->string 'x)))
(define-syntax-rule (schemevar x) (schemevarfont (symbol->string 'x)))

(define (shape . content) (apply racketcommentfont content))
(define (type . content) (apply racketcommentfont content))

(define-syntax-rule (define-declare-X declare-X formatter)
  (... (define-syntax-rule (declare-X id ...)
         (begin (define-syntax id
                  (make-element-id-transformer
                   (lambda _ #'(formatter (symbol->string 'id)))))
                ...))))

(define-declare-X declare-keyword racketkeywordfont)

(define-syntax ==>
  (make-element-id-transformer (lambda _ #'(elem "⇒"))))

(define-syntax ___
  (make-element-id-transformer (lambda _ #'(racketvarfont "___"))))

(define-syntax Quote
  (make-element-id-transformer (lambda _ #'(racket quote))))

(define-syntax Syntax
  (make-element-id-transformer (lambda _ #'(racket syntax))))

(define-syntax HOLE
  (make-element-id-transformer (lambda _ #'(lightgrey "␣")))) ;; or □ ?

(define-syntax ELIDED
  (make-element-id-transformer (lambda _ #'(grey "····")))) ;; or ⋯⋯

(define (grey . content)
  (apply elem #:style (style #f (list (color-property "gray"))) content))
(define (lightgrey . content)
  (apply elem #:style (style #f (list (color-property "lightgray"))) content))

(define-syntax ALT
  (make-element-id-transformer (lambda _ #'(elem "|"))))

(define-syntax !
  (make-element-id-transformer (lambda _ #'(racketparenfont "."))))

(define-syntax expr_
  (make-element-id-transformer (lambda _ #'(racketidfont "expr"))))
(define-syntax fail_
  (make-element-id-transformer (lambda _ #'(racketidfont "fail"))))
(define-syntax datum_
  (make-element-id-transformer (lambda _ #'(racketidfont "datum"))))

;; ----

(define reference-doc '(lib "scribblings/reference/reference.scrbl"))
(define guide-doc '(lib "scribblings/guide/guide.scrbl"))

(define (tech/reference #:key [key #f] . pre-content)
  (apply tech #:key key #:doc reference-doc pre-content))
(define (tech/guide #:key [key #f] . pre-content)
  (apply tech #:key key #:doc guide-doc pre-content))

(define (secref/reference name)
  (secref #:doc reference-doc name))
(define (secref/guide name)
  (secref #:doc guide-doc name))

;; ----

(define (lesson . pre-content)
  (nested (bold "Lesson: ") (apply italic pre-content)))

(define (lesson* . pre-content)
  (nested #:style 'inset (bold "Lesson: ") (apply italic pre-content)))

(define (later . pre-content)
  (apply elem pre-content))

;; ----

(define exercise-counter 1)
(define exercise-tags (make-hash))

(define (exercise #:stars [stars 0] #:tag [tag #f] . pre-content)
  (define exnum (begin0 exercise-counter (set! exercise-counter (add1 exercise-counter))))
  (define maybe-soln-link
    (traverse-element
     (lambda (get put)
       (define html? (memq 'html (get 'scribble:current-render-mode 'text)))
       (make-delayed-element
        (lambda (renderer part info)
          ;; does the solution tag exist?
          (define soln?
            (and tag (resolve-get/tentative part info `(part ,(make-solution-tag tag)))))
          (if (and soln? html?)
              (elem ~ (seclink (make-solution-tag tag) (elem "[solution]")))
              (elem "")))
        (lambda () "")
        (lambda () "")))))
  (define stars-elem
    (if (>= stars 1)
        (elem #|smaller|# ~ "(" (make-string stars (integer->char 9733)) ")")
        (elem)))
  (define header (bold "Exercise" ~ (number->string exnum) stars-elem maybe-soln-link ": "))
  (define header* (if tag (make-target-element #f header (make-exercise-tag tag)) header))
  (when tag (hash-set! exercise-tags tag exnum))
  (list (tag-note tag) (apply nested header* pre-content)))

(define (tag-note tag)
  (if (and draft-mode? tag) (margin-note (format "tag = ~s" tag)) null))

(define STAR (make-string 1 (integer->char 9733)))

(define (exercise-number tag)
  (or (hash-ref exercise-tags tag #f)
      (error 'exercise-number "no exercise found with given tag\n  tag: ~e" tag)))

(define (exercise-ref tag)
  (define exnum (exercise-number tag))
  (make-link-element #f (elem "Exercise" ~ (number->string exnum)) (make-exercise-tag tag)))

(define (exercise-number-ref tag)
  (define exnum (exercise-number tag))
  (make-link-element #f (elem (number->string exnum)) (make-exercise-tag tag)))

(define (make-exercise-tag tag)
  `(exercise ,(doc-prefix #f #f tag)))

(define-syntax-rule (solution-section #:tag tag)
  (section #:tag (make-solution-tag tag)
           "Solution for " (exercise-ref tag)))

(define (make-solution-tag tag)
  (string-append "exercise-solution-" tag))

#;
;; Doesn't fully work, no way to avoid div wrapper on summary (I think)
(define (accordion summary . pre-flow)
  (compound-paragraph
   (style #f (list (alt-tag "details")))
   (cons (paragraph (style #f (list (alt-tag "summary")))
                    (decode-content (list summary)))
         (decode-flow pre-flow))))

(define license-link "https://creativecommons.org/licenses/by-nc-nd/4.0/")
(define big-licence-img "https://licensebuttons.net/l/by-nc-nd/4.0/88x31.png")
(define small-license-img "https://i.creativecommons.org/l/by-nc-nd/4.0/80x15.png")
(define-runtime-path local-license-img "static/by-nc-nd-4.0-80x15.png")
(define license-name "Attribution-NonCommercial-NoDerivatives 4.0 International")


(define (cc-footer)
  (paragraph
   (style #f (list (attributes
                    '((style . "padding: 4em 0")))))
   (cc-elem)))

(define (cc-elem)
  (define llink-style (style #f (list (attributes '((rel . "license"))))))
  (define (llink e) (hyperlink #:style llink-style license-link e))
  @elem{@llink[@image[local-license-img]{Creative Commons License}]
        This work by Ryan Culpepper is licensed under a
        @llink[@elem{@license-name License}].})

(define (make-malr-eval #:assert? [assert? #t])
  (define the-eval (make-base-eval))
  (the-eval '(require (only-in racket/base [quote Quote] [syntax Syntax])
                      rackunit syntax/macro-testing racket/block
                      (for-syntax racket/base racket/syntax syntax/parse syntax/datum
                                  (only-in racket/base [quote Quote] [syntax Syntax]))))
  (when assert?
    (the-eval '(define-syntax assert
                 (syntax-parser
                   [(_ condition:expr)
                    #'(unless condition
                        (error 'assert "failed: ~s" (quote condition)))]))))
  the-eval)
