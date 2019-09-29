#lang racket
(require pict)

(module reader syntax/module-reader
  dtc/hello/colors)

(provide print
         (all-from-out racket))

(define (pick l)
  (first (shuffle l)))

(define (print s #:bg (bg (pick '(red green blue cyan purple magenta))))
  (cc-superimpose
    (filled-rectangle 100 100 #:color (~a bg))
    (text (~a s)))) 
