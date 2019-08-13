#lang racket

(require 2htdp/image)

(module reader syntax/module-reader
  dtc/hello/animation)

(provide print print-image
         (all-from-out racket))

(define (print-image s)
 (overlay (text s 40 'red)
  (circle 100 'solid 'white)))




(define (print s) 
  (define spin
    (dynamic-require '"./animation/spin.rkt" 'spin))

  (spin (print-image s)))


(module+ test
  (print "Hello"))
