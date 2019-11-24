#lang racket
(require 2htdp/image)

(module reader syntax/module-reader
  dtc/hello/colors)

(provide print
         (all-from-out racket))

(define (pick l)
  (first (shuffle l)))

(define shapes
  (list square circle star))

(define (print s 
               #:shape (shape (pick shapes))
               #:bg (bg (pick '(red green blue cyan purple magenta))))
  (overlay
    (text (~a s) 24 'black)   
    (shape 100 'solid bg)))
