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
     (display "'()")]))

(define (sumNode tree)
  (cond
    [(node? tree)
     (+ (node-value tree) (sumNode (node-left tree)) (sumNode (node-right tree)))]
    [else 0]))
(define tree 
  (node 10 
        (node 22 '() '())
        (node 3 
        (node 4 '() '()) '())) )  
(display "Tree: ")
(illu tree)
(newline)

(display "Sum of all node values: ")
(display (sumNode tree))
(newline)


