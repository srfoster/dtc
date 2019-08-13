#lang racket

(provide spin)

(require meta-engine)
(define (spin image)
  (play! 
    (game 
      (entity (sprite (register-sprite image))
              (position (posn 200 200))
              (rotation 0 (^ (curry + 0.01))))))) 


