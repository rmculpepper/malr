#lang racket/base
(require (for-syntax racket/base)
         (for-syntax syntax/parse))
(require scribble/basic
         scribble/struct
         scribble/manual
         scribble/decode)
(provide schemekw
         schemevar
         bq)

(define-syntax-rule (schemekw x) (schemekeywordfont (symbol->string 'x)))
(define-syntax-rule (schemevar x) (schemevarfont (symbol->string 'x)))

(define (bq . content)
  (make-blockquote #f (flow-paragraphs (decode-flow content))))
