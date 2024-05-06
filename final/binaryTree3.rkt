#lang racket

(struct node (value left right))

(define (illu tree)
  (cond
    [(node? tree)
     (display "(")
     (display (node-value tree))
     (display " ")
     (illu (node-left tree))
     (display " ")
     (illu (node-right tree))
     (display ")")]
    [else
     (display "empty")]))

(define (inserIntoTree value tree)
  (cond
    [(node? tree)
     (if (< value (node-value tree))
         (node (node-value tree) (inserIntoTree value (node-left tree)) (node-right tree))
         (node (node-value tree) (node-left tree) (inserIntoTree value (node-right tree))))]
    [else (node value '() '())]))

(define numbers '(1 2 3 4 5 6 7))

(define tree (foldl inserIntoTree '() numbers))

(illu tree)

