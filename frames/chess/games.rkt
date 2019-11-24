#lang racket

(provide 
  napoleon/turk
  napoleon/turk-raw)

(require "./chess.rkt")

(require (prefix-in napoleon/turk: "./games/napoleon-turk-1809.rkt") 
         chess)

(define (chess->list c)
  (for*/list ([r (in-chess-ranks #:descending? #t)]
              [f (in-chess-files)])
    (piece->symbol
      (chess-board-ref c 
                       (chess-square #:rank r #:file f)))))

(define (piece->symbol cp)
  (define p (and cp 
                 (colored-chess-piece-type cp)))

  (define s
    (cond
      [(not p) "_"]
      [(equal? king p) "k"]
      [(equal? queen p) "q"]
      [(equal? bishop p) "b"]
      [(equal? knight p) "n"]
      [(equal? rook p) "r"]
      [(equal? pawn p) "p"]))

  (string->symbol
    (if (and cp (equal? black (colored-chess-piece-owner cp)))
      (string-upcase s)   
      s )))

(define napoleon/turk-full
  (map chess->list napoleon/turk:moves))

(define (napoleon/turk n)
  (image-chess (napoleon/turk-raw n)))

(define (napoleon/turk-raw n)
  (list-ref napoleon/turk-full n))

(module+ test
  (map image-chess napoleon/turk-full))
