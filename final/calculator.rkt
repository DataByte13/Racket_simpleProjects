#lang racket
;***********************************
;***********************************
; power section : 
(define (power base exponent)
  (if (= exponent 0)
    1 
    (* base (power base (- exponent 1)))
    )
)
(define (sum num1 num2)
  (+ num1 num2)
)
(define (sub num1 num2)
  (- num1 num2)
)
(define (dev num1 num2)
  (/ num1 num2)
)
(define (mult num1 num2)
  (* num1 num2)
)
;***************************************************************************************
;-------------Radical section-------------------------- 
(define (Rad base nth_root)
  (define root 1000.0)
  (define periv_root 0)
  (define counter 0)
  
  (define (gess groot nth_root)
    (displayln "iam here to change :")
    (displayln groot)
    (displayln base)
    (- groot (/ (- (power groot nth_root) (* base 1.0))(* nth_root (power groot (- nth_root 1)))))
  )
  (define (test_gess root periv_root)
    (displayln "iam here to comp")
    (displayln root)
    (displayln periv_root)
    (if (< (abs(- root periv_root)) 0.001)
      1
      -1
      )
    )

  (define (while counter)
    (cond 
      [(< counter 2)
      (set! periv_root root)
      (set! root (gess root nth_root))
      (set! counter (+ counter (test_gess root periv_root)))
      (while counter)]
      [else root]
    )
  )
  
  (if (= counter 2)
    root
    (while counter)
  ))

(define (derivative f)
  (lambda (x)
    (define dx 0.000001)
    (/ (- (f (+ x dx)) (f x)) dx)))

(define (symbolic-derivative f)
  (lambda (x)
    (define expr (f 'x))
    (define derived-expr (derivative f))
    (let ((derived-value (derived-expr x)))
      `(,expr ,derived-value))))

(define (f x)
  `(+ ,x ,x)) ; Define the function f(x) = x + x

(define f-dev (symbolic-derivative f))
(displayln (f-dev 1)) ; Display the symbolic derivative at x = 1

(define (g x)
  `(* ,x ,x)) ; Define the function g(x) = x * x

(define g-dev (symbolic-derivative g))
(displayln (g-dev 1)) ; Display the symbolic derivative at x = 1

(define contains-x?
  (lambda (equation)
    (cond
      [(not (list? equation)) (eq? equation 'x)]
      [(null? equation) #f]
      [else (or (contains-x? (car equation)) (contains-x? (cdr equation)))])))

(define evaluate
  (lambda (equation value)
    (cond
      [(number? equation) equation]
      [(eq? equation 'x) value]
      [(not (list? equation)) (error "error: evaluate - bad input")]
      [else (let ([eq (replace-x equation value)])
              (evaluate-rec eq))])))

(define replace-x
  (lambda (equation value)
    (map (lambda (i) (cond
                       [(eq? i 'x) value]
                       [(list? i) (replace-x i value)]
                       [else i])) equation)))

(define evaluate-rec
  (lambda (equation)
    (cond
      [(number? equation) equation]
      [(not (list? equation)) (error "error: evaluate-rec - bad input")]
      [(null? equation) 0]
      [(eq? (car equation) '+) (cond
                                 [(null? (cddr equation)) (evaluate-rec (cadr equation))]
                                 [else (+ (evaluate-rec (cadr equation))
                                          (evaluate-rec (cons '+ (cddr equation))))])]
      [(eq? (car equation) '*) (cond
                                 [(null? (cddr equation)) (evaluate-rec (cadr equation))]
                                 [else (* (evaluate-rec (cadr equation))
                                          (evaluate-rec (cons '* (cddr equation))))])]
      [(eq? (car equation) '/) (cond
                                 [(null? (cddr equation)) (evaluate-rec (cadr equation))]
                                 [else (/ (evaluate-rec (cadr equation))
                                          (evaluate-rec (cons '* (cddr equation))))])]
      [(eq? (car equation) 'sin) (sin (evaluate-rec (cadr equation)))]
      [(eq? (car equation) 'cos) (cos (evaluate-rec (cadr equation)))]
      [(eq? (car equation) 'tan) (tan (evaluate-rec (cadr equation)))]
      [(eq? (car equation) 'csc) (/ 1 (sin (evaluate-rec (cadr equation))))]
      [(eq? (car equation) 'sec) (/ 1 (cos (evaluate-rec (cadr equation))))]
      [(eq? (car equation) 'cot) (/ 1 (tan (evaluate-rec (cadr equation))))]
      [(eq? (car equation) '^) (expt (evaluate-rec (cadr equation)) (evaluate-rec (caddr equation)))]
      [else (error (string-append "error: evaluate-rec - \""
                                  (symbol->string (car equation))
                                  "\" is not supported"))])))

(define contains?
  (lambda (equation target)
    (cond
      [(not (list? equation)) (eq? equation target)]
      [(null? equation) #f]
      [else (or (contains? (car equation) target) (contains? (cdr equation) target))])))

(define integral-rec
  (lambda (equation)
    (cond
      [(eq? equation 0) 0]
      [(number? equation) (list '* equation 'x)]
      [(eq? equation 'x) (list '* 1/2 (list '^ 'x 2))]
      [(not (list? equation)) (error "error: integral-rec - bad input")]
      [(null? equation) (error "error: integral-rec - bad input")]
      [(eq? 1 (length equation)) (cond
                                   [(eq? (car equation) 0) 0]
                                   [(number? (car equation)) (list '* (car equation) 'x)]
                                   [(eq? (car equation) 'x) (list '* 1/2 (list '^ 'x 2))]
                                   [else (error "error: integral-rec - bad input")])]
      [(eq? (car equation) '+) (cond
                                 [(not (contains-x? equation)) (integral-rec (evaluate-rec equation))]
                                 [(null? (cddr equation)) (integral-rec (cadr equation))]
                                 [else (list '+
                                             (integral-rec (cadr equation))
                                             (integral-rec (cons '+ (cddr equation))))])]
      [(eq? (car equation) '-) (cond
                       [(null? (cddr equation)) (list '* -1 (integral-rec (cadr equation)))]
                       [else (integral-rec (cons '+ (cons (cadr equation) (map (lambda (i)
                                                                     (list '* -1 i)) (cddr equation)))))])]
      [(eq? (car equation) '*) (cond
                                 [(not (contains-x? equation)) (integral-rec (evaluate-rec equation))]
                                 [(null? (cddr equation)) (integral-rec (cadr equation))]
                                 [(and (contains-x? (cadr equation))
                                       (contains-x? (cons '*
                                                          (cddr equation)))) (error "error: integral-rec - integration by parts not supported")]
                                 [(not (contains-x? (cadr equation))) (list '*
                                                                            (evaluate-rec (cadr equation))
                                                                            (integral-rec (cons '* (cddr equation))))]
                                 [(not (contains-x? (cons '* (cddr equation)))) (list '*
                                                                                      (evaluate-rec (cons '* (cddr equation)))
                                                                                      (integral-rec (cadr equation)))]
                                 [else (error "error: integral-rec - bad input")])]
      [(eq? (car equation) '^) (cond
                                 [(not (contains-x? equation)) (integral-rec (evaluate-rec equation))]
                                 [(and (and (list? (cadr equation))
                                            (eq? (caadr equation) 'sec)
                                            (eq? (cadadr equation) 'x))
                                       (eq? (caddr equation) 2)) (list 'tan 'x)]
                                 [(contains-x? (caddr equation)) (error "error: integral-rec - exponents containing x are not supported")]
                                 [(contains-x? (cadr equation)) (cond
                                                                  [(contains-trig? (cadr equation)) (error "error: integral-rec - trig functions are not supported")]
                                                                  [(contains? (cadr equation) '/) (error "error: integral-rec - \"/\" is not supported in exponents")]
                                                                  [(and (not (list? (cadr equation)))
                                                                        (and (not (eq? (cadr equation) 'x))
                                                                            (not (number? (cadr equation))))) (error "error: integral-rec - unrecognized base of exponent")]
                                                                  [(or (eq? (cadr equation) 'x)
                                                                       (and (list? (cadr equation))
                                                                            (eq? (caadr equation) '*)
                                                                            (number? (cadadr equation))
                                                                            (eq? (car (cddadr equation)) 'x))
                                                                                                          ) (list '*
                                                                                                                  (/ (evaluate equation 1)
                                                                                                                     (+ 1
                                                                                                                        (evaluate-rec (caddr equation))))
                                                                                                                  (list '^
                                                                                                                        'x
                                                                                                                        (+ 1
                                                                                                                           (evaluate-rec (caddr equation)))))]
                                                                  [else (error "error: integral-rec - unsupported base for exponent")])]
                                 [else (integral-rec (evaluate-rec equation))])]
      [(eq? (car equation) 'sin) (cond
                                   [(not (contains-x? equation)) (evaluate-rec equation)]
                                   [(eq? (cadr equation) 'x) (list '* -1 (list 'cos 'x))]
                                   [else (error "error: integral-rec - integration by parts not supported")])]
      [(eq? (car equation) 'cos) (cond
                                   [(not (contains-x? equation)) (evaluate-rec equation)]
                                   [(eq? (cadr equation) 'x) (list 'sin 'x)]
                                   [else (error "error: integral-rec - integration by parts not supported")])]
      [else (error (string-append "error: integral-rec - \""
                                  (symbol->string (car equation))
                                  "\" is not supported"))])))

(define contains-trig?
  (lambda (equation)
    (or (contains? equation 'sin)
        (contains? equation 'cos)
        (contains? equation 'tan)
        (contains? equation 'csc)
        (contains? equation 'sec)
        (contains? equation 'cot))))

(define (is-exponential? equation)
  (and (list? equation)
       (= (length equation) 3)
       (eq? (car equation) '^)
       (number? (cadr equation))
       (eq? (caddr equation) 'x)))

(define (integrate-exponential equation)
  (let* ((base (cadr equation))
         (new-exponent (+ (caddr equation) 1))
         (new-expression (list '/ (list '^ base 'x) new-exponent)))
    new-expression))

(integral-rec '(+ (* 5 (^ (* 2 x) 3)) (* 3 x) 2))
(integral-rec '(^ x 2))
