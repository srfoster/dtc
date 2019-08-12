#lang racket

(provide (except-out 
           (all-from-out racket)
           #%module-begin)
         (rename-out 
           [my-begin #%module-begin]))

(require pict )
(require (for-syntax racket
                     "./arrows/parsing.rkt"  ))

(module reader syntax/module-reader
  dtc/story/arrows)

(define-syntax-rule (quote-all s ...)
  (begin 's ...))

(define-syntax (my-begin stx)
  (define tokens (rest (syntax->datum stx)))

  (define stories
    (tokens->stories tokens) )

  #`(#%module-begin
     (quote-all
       #,@stories) ))
