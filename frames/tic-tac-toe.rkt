#lang racket

(provide image-tic-tac-toe)

(require 2htdp/image)

(define (image-tic-tac-toe b)
  (define is (map symbol->image b))   
  (define r1 (apply row (take is 3)))
  (define r2 (apply row (take (drop is 3) 3)))
  (define r3 (apply row (take (drop is 6) 3)))
 
  (pad
    (above-space r1 r2 r3)))

(define blank
  (square 50 'solid 'white))

(define (symbol->image s)
  (cond
    [(image? s) (scale-to-fit 50 s)]
    [(eq? '_ s) blank]
    [else (overlay (text (~a s) 25 'black)
                   blank
                   )]))

(define (row a b c)
  (beside-space a b c))

(define space (circle 5 'solid 'transparent))

(define (beside-space . is)
  (apply beside (add-between is space)))

(define (above-space . is)
  (apply above (add-between is space)))

(define (pad i)
  (overlay
    i
    (rectangle
      (image-width i)  
      (image-height i)  
      'solid
      'black)))

(define (scale-to-fit n i)
  (scale/xy (/ n (image-width i))
            (/ n (image-height i))
            i))
