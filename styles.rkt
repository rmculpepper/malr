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
         tech/reference
         tech/guide
         lesson
         lesson*
         exercise)

(define-syntax-rule (schemekw x) (schemekeywordfont (symbol->string 'x)))
(define-syntax-rule (schemevar x) (schemevarfont (symbol->string 'x)))

(define-syntax-rule (define-declare-X declare-X formatter)
  (... (define-syntax-rule (declare-X id ...)
         (begin (define-syntax id
                  (make-element-id-transformer
                   (lambda _ #'(formatter (symbol->string 'id)))))
                ...))))

(define-declare-X declare-keyword racketkeywordfont)

;; ----

(define reference-doc '(lib "scribblings/reference/reference.scrbl"))
(define guide-doc '(lib "scribblings/guide/guide.scrbl"))

(define (tech/reference #:key [key #f] . pre-content)
  (apply tech #:key key #:doc reference-doc pre-content))
(define (tech/guide #:key [key #f] . pre-content)
  (apply tech #:key key #:doc guide-doc pre-content))

;; ----

(define (lesson . pre-content)
  (nested (apply italic (bold "Lesson: ") pre-content)))

(define (lesson* . pre-content)
  (nested #:style 'inset (apply italic (bold "Lesson: ") pre-content)))

;; ----

(define exercise-counter 1)

(define (exercise . pre-content)
  (begin0 (apply nested (bold (format "Exercise ~s: " exercise-counter)) pre-content)
    (set! exercise-counter (add1 exercise-counter))))
