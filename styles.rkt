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
         declare-keyword)

(define-syntax-rule (schemekw x) (schemekeywordfont (symbol->string 'x)))
(define-syntax-rule (schemevar x) (schemevarfont (symbol->string 'x)))

(define-syntax-rule (define-declare-X declare-X formatter)
  (... (define-syntax-rule (declare-X id ...)
         (begin (define-syntax id
                  (make-element-id-transformer
                   (lambda _ #'(formatter (symbol->string 'id)))))
                ...))))

(define-declare-X declare-keyword racketkeywordfont)
