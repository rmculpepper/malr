#lang racket/base
(require (for-syntax racket/base)
         (for-syntax syntax/parse))
(require scribble/basic
         ;; scribble/struct
         scribble/manual
         scribble/decode
         scribble/racket
         scribble/core)
(provide schemekw
         schemevar
         declare-keyword
         ___
         ==>
         tech/reference
         tech/guide
         lesson
         lesson*
         later
         exercise
         exercise-number
         exercise-ref
         solution)

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

(define (exercise #:tag [tag #f] . pre-content)
  (define exnum (begin0 exercise-counter (set! exercise-counter (add1 exercise-counter))))
  (define maybe-soln-link
    (make-delayed-element
     (lambda (renderer part info)
       ;; does the solution tag exist?
       (define soln? (resolve-get/tentative part info `(exercise-solution ,tag)))
       (if soln?
           (elem ~ (make-link-element #f (elem "[solution]") (make-solution-tag tag)))
           (elem "")))
     (lambda () "")
     (lambda () "")))
  (define header (bold "Exercise" ~ (number->string exnum) maybe-soln-link ": "))
  (define header* (if tag (make-target-element #f header (make-exercise-tag tag)) header))
  (when tag (hash-set! exercise-tags tag exnum))
  (apply nested header* pre-content))

(define (exercise-number tag)
  (or (hash-ref exercise-tags tag #f)
      (error 'exercise-number "no exercise found with given tag\n  tag: ~e" tag)))

(define (exercise-ref tag)
  (define exnum (exercise-number tag))
  (make-link-element #f (format "Exercise ~s" exnum) (make-exercise-tag tag)))

(define (make-exercise-tag tag)
  `(exercise ,(doc-prefix #f #f tag)))

(define-syntax-rule (solution-section #:tag tag)
  (section #:tag `(exercise-solution ,tag)
           "Solution for " (exercise-ref tag)))

(define (solution #:tag tag . pre-content)
  (define header (bold "Solution for " (exercise-ref tag) ": "))
  (define header* (make-target-element #f header (make-solution-tag tag)))
  (apply nested header* pre-content))

(define (make-solution-tag tag)
  `(exercise-solution ,(doc-prefix #f #f tag)))
