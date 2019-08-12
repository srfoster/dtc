#lang racket

(module reader syntax/module-reader
  dtc/hello/normal)

(provide print
         (all-from-out racket))

(define (print s)
  s) 
