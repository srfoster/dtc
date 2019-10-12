#lang racket

(require pict/code
         syntax/parse/define
         (only-in pict pict->bitmap))

(provide image-code)

(define-syntax (image-code stx)
  #`(pict->bitmap
      #,(syntax-parse stx
          [(_ (quote datum)) 
           #`(typeset-code (syntax datum))]
          [(_ (quasiquote datum)) 
           #`(typeset-code (syntax datum))]
          [(_ datum) 
           #`(typeset-code (syntax datum))])))

