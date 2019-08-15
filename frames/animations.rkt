#lang racket

(provide
 (except-out 
  (all-from-out racket)
#%module-begin)
 (rename-out 
  [my-begin #%module-begin]))

(module reader syntax/module-reader
  dtc/frames/animations)

(define (animation . stuff)
  ;TODO: support a bunch of different types.  Construct meta-game slideshow thingy
  (void))

(define-syntax-rule (my-begin expr ...)
  (#%module-begin 
   expr ...))


(provide (all-from-out "./chess/chess.rkt"))
(require "./chess/chess.rkt")

(provide (all-from-out "./chess/games.rkt"))
(require "./chess/games.rkt")
