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

(define tree
  (node 1
        (node 2
          (node 4 '() '())
          '()
        )
        (node 3
          (node 5 '() '())
          (node 6 '() '())
        )
  ))

(illu tree)

