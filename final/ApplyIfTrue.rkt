#lang racket

(define (applyIfTrue condition func value)
  (if (condition value)
      (func value)
      value))

(define (greaterThenZero x)
  (< 0 x))


(define (square x)
  (* x x))

(displayln (applyIfTrue even? square 3)) 
(displayln (applyIfTrue even? square 4)) 
(displayln (applyIfTrue greaterThenZero square -1))

