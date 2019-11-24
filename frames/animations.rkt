#lang racket

(provide
 (except-out 
  (all-from-out racket)
#%module-begin)
 (rename-out 
  [my-begin #%module-begin]))

(module reader syntax/module-reader
  dtc/frames/animations)


;ANIMATION

(provide (all-from-out "./animations/main.rkt"))
(require "./animations/main.rkt")

;Code Images

(provide (all-from-out "./image-code.rkt"))
(require "./image-code.rkt")



;Misc and reprovides

;Um do we need this?
(define-syntax-rule (my-begin expr ...)
  (#%module-begin 
   expr ...))


(provide (all-from-out "./chess/chess.rkt"))
(require "./chess/chess.rkt")

(provide (all-from-out "./chess/games.rkt"))
(require "./chess/games.rkt")

(provide (all-from-out "./story-images/main.rkt"))
(require "./story-images/main.rkt")

(provide (all-from-out "../story/cats.rkt"))
(require (except-in
            "../story/cats.rkt"
            #%module-begin))


(provide (all-from-out "./tic-tac-toe.rkt"))
(require "./tic-tac-toe.rkt")

