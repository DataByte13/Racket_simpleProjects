#lang racket

(struct node (value left right))
(define (compare-trees tree1 tree2)
  (cond
    [(and (empty? tree1) (empty? tree2)) #t] 
    [(and (node? tree1) (node? tree2)) 
     (and (= (node-value tree1) (node-value tree2))
          (compare-trees (node-left tree1) (node-left tree2))
          (compare-trees (node-right tree1) (node-right tree2)))]
    [else #f])
) 

(define tree1
  (node 10
        (node 5 '() '())
        (node 15 '() '()
  )
))

(define tree2
(node 1 (node 2 empty empty) (node 4 (node 4 empty empty) empty))
)
(define tree3
(node 10 (node 22 empty empty) (node 4 empty empty))
)

(display "comp tree1 and tree2  ")
(display (compare-trees tree1 tree2))
(newline)

(display "comp tree1 and tree3  ")
(display (compare-trees tree1 tree3))
(newline)

