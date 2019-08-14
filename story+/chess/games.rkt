#lang racket

(require "./chess.rkt")

(define napoleon/turk-full
  (list
    starter 

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

    '(R N B Q K B _ R
      P P P P _ P P P
      _ _ _ _ _ N _ _
      _ _ _ _ P _ _ _
      _ _ _ _ p _ _ _ 
      _ _ _ _ _ q _ _
      p p p p _ p p p
      r n b _ k b n r)))

(module+ test
  (map image-chess napoleon/turk-full))
