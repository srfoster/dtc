#lang racket

;Lang for making silly cat images.

(provide normal-begin
         (except-out 
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

(define-syntax-rule (normal-begin e ...)
  (#%module-begin e ...))

;Actual cat lang below.  TODO: Move to new file when this gets too long.  

(provide cat 
 rotate 
 rotate-left
 shrink
 edison-cat
 authors-cat
 first-cat-photo 
 first-viral-cat
 
 redify

 meme-teacher
 dtc-cover
 dijkstra
 habermann
 notkin
 griswold)

(require racket/runtime-path)

(define-runtime-path cat-path "./cats")

(define (load-cat png-name)
 (h:scale 0.2
  (h:bitmap/file (build-path cat-path "img" png-name))))

(define (cat . params)
  (apply cat-main (load-cat "maybe-first-cat-photo.png") params))

(define (first-cat-photo . params)
  (apply cat-main (load-cat "maybe-first-cat-photo.png") params))

(define (first-viral-cat . params)
  (apply cat-main (load-cat "first-viral-cat.png") params))

(define (edison-cat . params)
  (apply cat-main (load-cat "edison-cat.png") params))

(define (authors-cat . params)
  (apply cat-main 
         (load-cat "kitty.png") 
         params))


;Other cool cats :)

(define (dijkstra . params)
  (apply cat-main 
         (load-cat "dijkstra.png") 
         params))

(define (habermann . params)
  (apply cat-main 
         (load-cat "habermann.png") 
         params))

(define (notkin . params)
  (apply cat-main 
         (load-cat "notkin.png") 
         params))

(define (griswold . params)
  (apply cat-main 
         (load-cat "griswold.png") 
         params))

(define (meme-teacher . params)
  (apply cat-main 
         (load-cat "meme-teacher.jpg") 
         params))
         
(define (dtc-cover . params)
  (apply cat-main 
         (load-cat "dtc-cover.png") 
         params))

(define (rotate i)
  (define actual-i (if (procedure? i) (i) i))
  (h:rotate -45 actual-i))

(define (rotate-left i)
  (define actual-i (if (procedure? i) (i) i))
  (h:rotate 45 actual-i))

(define (shrink i)
  (define actual-i (if (procedure? i) (i) i))
  (h:scale 1/2 actual-i))

(define (redify i)
  (define actual-i (if (procedure? i) (i) i))
  (h:overlay
    (h:text "TODO: Red" 24 'red)
    actual-i))

(define (cat-main i . params)
  ((apply compose 
          (cons identity 
                (reverse params)))
  i)) 






