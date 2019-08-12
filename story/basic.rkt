#lang racket

(provide (except-out 
           (all-from-out racket)
           #%module-begin)
         (rename-out 
           [my-begin #%module-begin]))

(require pict)

(module reader syntax/module-reader
  dtc/story/basic)


(define (right-arrows . is)
  (define arrows (map (thunk* (arrow 15 0))
                      (range (length is))))

  (define all
    (drop-right (flatten (map list is arrows)) 1))

  (apply hc-append all))

(define (datum->node e)
  (define t (text (~a e)) )
  (frame (inset t 5) 
         #:color "black" #:line-width 3))

(define (datum->story e)
  (apply right-arrows
         (map datum->node e)))

(define-syntax-rule (story expr)
  (datum->story 'expr))

(define-syntax-rule (my-begin expr ...)
  (#%module-begin 
   (story expr) ...))
