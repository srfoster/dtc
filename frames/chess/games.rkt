#lang racket

(provide 
  napoleon/turk
  napoleon/turk-raw)

(require "./chess.rkt")

(define napoleon/turk-full
  (list
    chess-start 

    '(R N B Q K B N R
      P P P P P P P P
      _ _ _ _ _ _ _ _
      _ _ _ _ _ _ _ _
      _ _ _ _ p _ _ _ 
      _ _ _ _ _ _ _ _
      p p p p _ p p p
      r n b q k b n r)

    '(R N B Q K B N R
      P P P P _ P P P
      _ _ _ _ _ _ _ _
      _ _ _ _ P _ _ _
      _ _ _ _ p _ _ _ 
      _ _ _ _ _ _ _ _
      p p p p _ p p p
      r n b q k b n r)

    '(R N B Q K B N R
      P P P P _ P P P
      _ _ _ _ _ _ _ _
      _ _ _ _ P _ _ _
      _ _ _ _ p _ _ _ 
      _ _ _ _ _ q _ _
      p p p p _ p p p
      r n b _ k b n r)

    '(R _ B Q K B N R
      P P P P _ P P P
      _ _ N _ _ _ _ _
      _ _ _ _ P _ _ _
      _ _ b _ p _ _ _ 
      _ _ _ _ _ q _ _
      p p p p _ p p p
      r n b _ k _ n r)))

(define (napoleon/turk n)
  (image-chess (napoleon/turk-raw n)))

(define (napoleon/turk-raw n)
  (list-ref napoleon/turk-full n))

(module+ test
  (map image-chess napoleon/turk-full))
