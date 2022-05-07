#lang racket/base
(require (for-syntax racket/base)
         (for-syntax syntax/parse))
(require scribble/basic
         ;; scribble/struct
         scribble/manual
         scribble/decode
         scribble/racket
         scribble/core
         scribble/html-properties
         scribble/example
         (for-label racket/base))
(provide schemekw
         schemevar
         declare-keyword
         ___
         ==>
         Quote
         Syntax
         ALT
         !
         STAR
         tech/reference
         tech/guide
         lesson
         lesson*
         later
         exercise
         exercise-number
         exercise-ref
         exercise-number-ref
         solution-section)

(define-syntax-rule (schemekw x) (schemekeywordfont (symbol->string 'x)))
(define-syntax-rule (schemevar x) (schemevarfont (symbol->string 'x)))

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
