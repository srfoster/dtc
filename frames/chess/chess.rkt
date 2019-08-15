#lang racket

(provide chess-start image-chess)

(require 2htdp/image racket/runtime-path "./base.rkt")

(define-runtime-path img "./img")


(define (image-chess pieces)
  (define r1 (take (drop pieces (* 8 0)) 8))
  (define r2 (take (drop pieces (* 8 1)) 8))
  (define r3 (take (drop pieces (* 8 2)) 8))
  (define r4 (take (drop pieces (* 8 3)) 8))
  (define r5 (take (drop pieces (* 8 4)) 8))
  (define r6 (take (drop pieces (* 8 5)) 8))
  (define r7 (take (drop pieces (* 8 6)) 8))
  (define r8 (take (drop pieces (* 8 7)) 8))

  (define foreground
    (above
     (apply beside (map render-thing r1))
     (apply beside (map render-thing r2))
     (apply beside (map render-thing r3))
     (apply beside (map render-thing r4))
     (apply beside (map render-thing r5))
     (apply beside (map render-thing r6))
     (apply beside (map render-thing r7))
     (apply beside (map render-thing r8))))

  (overlay foreground (bg)))

(define (render-thing s)
  (if (eq? '_ s)
      (square 20 'solid 'transparent)
      (render-piece s)))

(define (uppercase? s)
  (regexp-match #rx"[A-Z]" s))

(define (render-piece s)
  (define letter (~a s))
  (define color (if (uppercase? letter)
                    "black"
                    "white"))

  (define symbol (lookup-symbol letter))


  (define piece (text symbol 15 "black"))

  (define scaled-piece
    (scale/xy
     1
     (/ 20 (image-height piece))
     piece))
  
  (overlay
   scaled-piece 
   (square 20 'solid 'transparent)))

(define (lookup-symbol s)
  (cond
    [(string=? "K" s) "♚"]
    [(string=? "Q" s) "♛"]
    [(string=? "B" s) "♜"]
    [(string=? "N" s) "♝"]
    [(string=? "R" s) "♞"]
    [(string=? "P" s) "♟"]

    [(string=? "k" s) "♔"]
    [(string=? "q" s) "♕"]
    [(string=? "b" s) "♖"]
    [(string=? "n" s) "♗"]
    [(string=? "r" s) "♘"]
    [(string=? "p" s) "♙"]))

(define (bg)
  (define whites
    (map (thunk* (square 20 "solid" "white")) (range 4)))

  (define blacks
    (map (thunk* (square 20 "solid" "gray")) (range 4)))

  (define r1 (apply beside (flatten (map list blacks whites))))
  (define r2 (apply beside (flatten (map list whites blacks))))

  (above r1 r2 r1 r2 r1 r2 r1 r2))

(module+ test
  (scale 2 (image-chess (shuffle chess-start))))

