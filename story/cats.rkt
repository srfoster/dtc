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
                     "./arrows/parsing.rkt"  )
         image-coloring)

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
 blueify
 greenify
 orangeify
 purpleify
 yellowify

 meme-teacher
 dtc-cover
 dijkstra
 habermann
 notkin
 griswold
 
 meso-star-1
 meso-star-2
 meso-star-3
 meso-star-4

 meso-sun-1
 meso-sun-2
 meso-sun-3
 meso-sun-4

 meso-rain-1
 meso-rain-2
 meso-rain-3
 meso-rain-4

 meso-fish-1
 meso-fish-2
 meso-fish-3
 meso-fish-4
 )

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


(define (meso-sun-1 . params)
  (apply cat-main 
         (load-cat "meso-sun-1.png") 
         params))

(define (meso-sun-2 . params)
  (apply cat-main 
         (load-cat "meso-sun-2.png") 
         params))

(define (meso-sun-3 . params)
  (apply cat-main 
         (load-cat "meso-sun-3.png") 
         params))

(define (meso-sun-4 . params)
  (apply cat-main 
         (load-cat "meso-sun-4.png") 
         params))


(define (meso-rain-1 . params)
  (apply cat-main 
         (load-cat "meso-rain-1.png") 
         params))

(define (meso-rain-2 . params)
  (apply cat-main 
         (load-cat "meso-rain-2.png") 
         params))

(define (meso-rain-3 . params)
  (apply cat-main 
         (load-cat "meso-rain-3.png") 
         params))

(define (meso-rain-4 . params)
  (apply cat-main 
         (load-cat "meso-rain-4.png") 
         params))


(define (meso-star-1 . params)
  (apply cat-main 
         (load-cat "meso-star-1.png") 
         params))

(define (meso-star-2 . params)
  (apply cat-main 
         (load-cat "meso-star-2.png") 
         params))

(define (meso-star-3 . params)
  (apply cat-main 
         (load-cat "meso-star-3.png") 
         params))

(define (meso-star-4 . params)
  (apply cat-main 
         (load-cat "meso-star-4.png") 
         params))

(define (meso-fish-1 . params)
  (apply cat-main 
         (load-cat "meso-fish-1.png") 
         params))

(define (meso-fish-2 . params)
  (apply cat-main 
         (load-cat "meso-fish-2.png") 
         params))

(define (meso-fish-3 . params)
  (apply cat-main 
         (load-cat "meso-fish-3.png") 
         params))

(define (meso-fish-4 . params)
  (apply cat-main 
         (load-cat "meso-fish-4.png") 
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
  (tint-image "red" actual-i))

(define (blueify i)
  (define actual-i (if (procedure? i) (i) i))
  (tint-image "blue" actual-i))

(define (greenify i)
  (define actual-i (if (procedure? i) (i) i))
  (tint-image "green" actual-i))

(define (purpleify i)
  (define actual-i (if (procedure? i) (i) i))
  (tint-image "purple" actual-i))

(define (yellowify i)
  (define actual-i (if (procedure? i) (i) i))
  (tint-image "yellow" actual-i))

(define (orangeify i)
  (define actual-i (if (procedure? i) (i) i))
  (tint-image "orange" actual-i))

(define (cat-main i . params)
  ((apply compose 
          (cons identity 
                (reverse params)))
  i)) 






