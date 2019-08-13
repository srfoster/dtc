#lang racket

(require meta-engine 2htdp/image)

(module reader syntax/module-reader
  dtc/hello/animation)

(provide print print-image
         (all-from-out racket))

(define (print-image s)
 (overlay (text s 40 'red)
  (circle 100 'solid 'white)))

(define (print s) 
  (spin (print-image s)))

(define (spin image)
  (play! 
    (game 
      (entity (sprite (register-sprite image))
              (position (posn 200 200))
              (rotation 0 (^ (curry + 0.01)))))))


(module+ test
  (print "Hello"))
