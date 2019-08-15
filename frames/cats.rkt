#lang racket

(provide 
 (all-from-out "../story/cats.rkt")
 (rename-out 
  [normal-begin #%module-begin]))

(module reader syntax/module-reader
  dtc/frames/cats)

(require (except-in "../story/cats.rkt" #%module-begin))

