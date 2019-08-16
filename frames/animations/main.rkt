#lang racket

(require meta-engine 
         2htdp/image)

(define delay 50)

(define (animate frames)
  (define es (map datum->entity 
                  frames
                  (range (length frames))))

  (play! 
    (game es)))

(define no-sprite (register-sprite (empty-scene 0 0)))

(define (datum->entity d i)
  (define d-sprite (register-sprite (datum->frame d)))
  (entity
    (counter 0 (^ add1))
    (position (posn 200 200)) 
    (rotation 0)
    (sprite no-sprite
            (cond 
                [(between (get-counter) 
                          (* i delay)
                          (* (add1 i) delay))
                 d-sprite]
                [else 
                  no-sprite]))))

(define (between n low hi)
  (and (>= n low)
       (< n hi)))

(define (datum->frame d)
  (cond
    [(image? d) d]
    [else (text (~a d) 50 'white)]))

(module+ test
  (animate 
    '(this is a test)) 
  
  (animate 
    `(this ,(circle 40 'solid 'red) a test)))

