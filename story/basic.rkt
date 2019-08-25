#lang racket

(provide 
 right-arrows
 datum->node
 datum->story
 datum->story-left

 (except-out 
  (all-from-out racket)
#%module-begin)
 (rename-out 
  [my-begin #%module-begin]))

(require 2htdp/image)

(module reader syntax/module-reader
  dtc/story/basic)

(define (frame #:color c #:line-width w i)
  (overlay 
    (rectangle (+ 5 (image-width i))
               (+ 5 (image-height i))
               'outline
               (pen c w "solid" "round" "round"))
    i))

(define (inset i n)
  (overlay 
    (rectangle (+ n (image-width i))
               (+ n (image-height i))
               'solid 'transparent) 
    i))

(define (arrow c)
  (beside (rectangle 10 2 'solid c) 
          (rotate 30 (triangle 10 'solid c))))

(define (add-arrows #:arrow a is)
  (define arrows (map (thunk* a)
                      (range (length is))))

  (define all
    (drop-right (flatten (map list is arrows)) 1))

  (if (= 1 (length all))
      (first all) 
      (apply beside all)))

(define (right-arrows #:color (c "black") . is)
  (add-arrows #:arrow (arrow c) is))

(define (left-arrows #:color (c "black") . is)
  (add-arrows #:arrow (rotate 180 (arrow c)) is))

(define (datum->node #:color (c "black") e)
  (define t 
    (if (or (image? e)) e
        (text (~a e) 14 'black) ))
  (frame (inset t 5) 
         #:color c #:line-width 3))

(define (datum->story #:color (c "black") e)
  (apply (curry right-arrows #:color c)
         (map (curry datum->node #:color c) e)))

(define (datum->story-left e)
  (apply left-arrows 
         (map datum->node e)))


(define-syntax-rule (story expr)
  (datum->story 'expr))

(define-syntax-rule (my-begin expr ...)
  (#%module-begin 
   (story expr) ...))
