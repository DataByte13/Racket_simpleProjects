#lang racket

(provide hamming-distance)
(define (hamming-distance str1 str2)
  (cond
    [(not 
       (= (string-length str1) (string-length str2)))
     " no equal length"]
    [else
     (for/sum ([i str1]
               [j str2])
       (if (char=? i j) 0 1))]
    ))
(hamming-distance "CTGAG" "GTAC")
