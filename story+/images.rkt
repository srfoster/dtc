#lang racket

(provide 
 (rename-out 
  [my-begin #%module-begin]))

(require (only-in "../story/basic.rkt" datum->story))

(module reader syntax/module-reader
  dtc/story+/images)

(define-syntax-rule (my-begin expr ...)
  (#%module-begin 
    (datum->story 'expr)
    ...))


