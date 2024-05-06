#lang racket
(define (reverse-list lst)
  (if (null? lst)
      (append lst lst)
      (append (reverse-list (cdr lst)) 
      (list (car lst))
  )))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) 
            (append (cdr l1) l2)
      )))
(displayln (reverse-list '(1 2 3 4 5)))

(displayln (reverse-list '(1)))
