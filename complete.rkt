#lang racket

(provide (all-from-out racket)
         (all-from-out "./frames/animations.rkt"))
(require racket
         (except-in "./frames/animations.rkt" #%module-begin))

(module reader syntax/module-reader
  dtc/complete)

