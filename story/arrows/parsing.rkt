#lang racket

(provide tokens->stories)

(define (arrow? d)
   (eq? '-> d))


(define (take-story-helper not-arrow? tokens)
  (cond 
    [(empty? tokens) '()] 
    [(and (not not-arrow?) 
          (arrow? (first (rest tokens)))) 
     '()]
    [else (cons (first tokens)
                (take-story-helper (not not-arrow?)
                                   (rest tokens)))]))

(define (take-story tokens)
  (take-story-helper #t tokens))

(define (drop-story tokens)
  (define next-story (take-story tokens))

  (drop tokens (length next-story)))

(define (tokens->stories tokens)
   (define next-story (take-story tokens))

   (if (not (empty? next-story))
     (cons (filter-not arrow? next-story)
           (tokens->stories (drop-story tokens)))
     '()))


(module+ test
  (require rackunit)  

  (check-equal?
    (take-story '(a -> b -> c d -> e -> f))
    '(a -> b -> c))
  
  (check-equal?
    (take-story (drop-story '(a -> b -> c d -> e -> f)))
    '(d -> e -> f))
  

  )



