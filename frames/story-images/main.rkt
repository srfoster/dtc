#lang racket

(provide image)

(require 2htdp/image
         (only-in "../../story/basic.rkt"
                  datum->story
                  datum->node
                  right-arrows))

(define (image s
               #:quoted (q #f)
               #:top (t #t))

  (define ret
    (cond
      [(and (list? s)
            (empty? s))
       (empty-scene 0 0)]
    

      [(quoted? s)
       (beside/align "top"
                     (tick)
                     (image #:quoted #t (second s)))]

      
      [(unquoted? s)
       (beside/align "bottom"
                     (comma)
                     (image #:quoted #f (second s)))]


      [(and (list? s)
            (not (lwl? s)))
       (datum->story #:color (if q "gray" "black") s)]


      
      
      [(not (list? s))
       (datum->node
        #:color (if q "gray" "black") s)]


  
      [else ;list that has a list inside...
       (apply (curry right-arrows #:color (if q "gray" "black"))
              (map (curry image
                          #:top #f
                          #:quoted q
                          ) s))]))

  (if (and (not t) (list? s))
      (datum->node  #:color (if q "gray" "black") ret)
      ret))

(define (quoted? l)
  (and (list? l)
       (not (empty? l))
       (or (eq? 'quote (first l))
           (eq? 'quasiquote (first l)))))

(define (unquoted? l)
  (and (list? l)
       (not (empty? l))
       (eq? 'unquote (first l))))


(define (lwl? d)
  (and (list? d)
       (findf list? d)))


(define (tick)
  (circle 5 'solid 'gray))

(define (comma)
  (circle 5 'solid 'black))



(module+ test
  (image
    '(this is a test))


  (image
    '(this is a (nested test)))


  (image
    '(this is a (nested (nested test))))


  (image
    '(this is a `(nested (nested test))))

  (image
    '(this is a `(nested `(nested test))))

  
  (image
    '(this is a `(nested ,(nested test))))

  (image
    '(this is a `(nested ,(circle 40 'solid 'red))))

  (image
    `(this is a (nested ,(circle 40 'solid 'red)))))
