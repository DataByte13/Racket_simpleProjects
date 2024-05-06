#lang racket

(define x 1)
(define y 1)
(define fib 1)
(define counter 0)

(define (fibo input)
  (for ([i input])
    (displayln fib)
    (cond
      [(= i counter) (displayln (+ y x))]
      [else
       (set! fib (+ x y))
       (set! y x)
       (set! x fib)]
    )
  )
)
(displayln "ignore first too number ! :)")
(fibo 10)

