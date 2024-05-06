#lang racket
(define (maximum lst)

  (cond
    [(null? lst) "empty"]
    [else (let ([max (car lst)])
         (findMax (cdr lst) max))]
    ))

(define (findMax newLst maxVal)
  (cond
    [(null? newLst) maxVal]
    [(> (car newLst) maxVal) (findMax (cdr newLst) (car newLst))]
    [else (findMax (cdr newLst) maxVal)]
    
  )
)
(displayln (maximum '(1 5 3 9 2)))
(displayln (maximum '(1.5 , 12.5 , 169.14)))
