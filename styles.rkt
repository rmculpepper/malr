;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: CC-BY-NC-ND-4.0

#lang at-exp racket/base
(require racket/runtime-path
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
         expr_
         fail_

         malr-version
         tech/reference
         tech/guide
         lesson
         lesson*
         later
         exercise
         exercise-number
         exercise-ref
         exercise-number-ref
         solution-section
         cc-footer)

(define malr-version "v2-draft-01")

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

(define-syntax ALT
  (make-element-id-transformer (lambda _ #'(elem "|"))))

(define-syntax !
  (make-element-id-transformer (lambda _ #'(racketparenfont "."))))

(define-syntax expr_
  (make-element-id-transformer (lambda _ #'(racketidfont "expr"))))
(define-syntax fail_
  (make-element-id-transformer (lambda _ #'(racketidfont "fail"))))

;; ----

(define reference-doc '(lib "scribblings/reference/reference.scrbl"))
(define guide-doc '(lib "scribblings/guide/guide.scrbl"))

(define (tech/reference #:key [key #f] . pre-content)
  (apply tech #:key key #:doc reference-doc pre-content))
(define (tech/guide #:key [key #f] . pre-content)
  (apply tech #:key key #:doc guide-doc pre-content))

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
  (apply nested header* pre-content))

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
