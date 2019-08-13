#lang racket

;Lang for making silly cat images.

(provide (except-out 
           (all-from-out racket)
           #%module-begin)
         (rename-out 
           [my-begin #%module-begin]))

(require (prefix-in h: 2htdp/image))
(require (for-syntax racket
                     "./arrows/parsing.rkt"  ))

(module reader syntax/module-reader
  dtc/story/cats)

(define-syntax (my-begin stx)
  (define tokens (rest (syntax->datum stx)))

  (define stories
    (tokens->stories tokens) )

  #`(#%module-begin
     #,@stories))


;Actual cat lang below.  TODO: Move to new file when this gets too long.  

(provide cat 
 rotate 
 first-cat-photo 
 first-viral-cat)

(require racket/runtime-path)

(define-runtime-path cat-path "./cats")


(define (first-viral-cat/base) 
 (h:scale 0.2
  (h:bitmap/file (build-path cat-path "img" "first-viral-cat.png"))))

(define (first-cat-photo/base) 
 (h:scale 0.2
  (h:bitmap/file (build-path cat-path "img" "maybe-first-cat-photo.png"))))

(define (cat . params)
  (apply cat-main (first-cat-photo/base) params))

(define (first-cat-photo . params)
  (apply cat-main (first-cat-photo/base) params))

(define (first-viral-cat . params)
  (apply cat-main (first-viral-cat/base) params))

(define (rotate i)
  (h:rotate -45 i))

(define (cat-main i . params)
  ((apply compose 
          (cons identity 
                (reverse params)))
  i)) 






