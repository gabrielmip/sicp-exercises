; exercise 1.4
(define (square a) (* a a))

(define (sum-squares a b) 
  (+ (square a) (square b)))

(define (square-sum-largest a b c)
  (cond
    ((and (>= a c) (>= b c)) (sum-squares a b))
    ((and (>= a b) (>= c b)) (sum-squares a c))
    (else (sum-squares c b))))

(square-sum-largest 3 6 1)

; exercise 1.5
; If the interpreter uses applicative-order evaluation,
; the expression will return 0, as it will not attempt
; to resolve (p). On the other hand, a normal-order evaluation
; will enter a never-ending cycle trying to recursively
; evaluate (p) to itself.

; topic 1.1.7 Newton's method
(define tolerance 0.001)

(define (sqrt a)
  
  (define (good-enough? guess a)
    (< (abs (- (square guess) a)) tolerance))

  (define (average a b)
    (/ (+ a b) 2))

  (define (improve-guess guess a)
    (average (/ a guess) guess))

  (define (sqrt-iter guess a)
    (if (good-enough? guess a)
      guess
      (sqrt-iter (improve-guess guess a) a)))

  (sqrt-iter 1.0 a))

(sqrt 2)


; exercise 1.7 good-enough considering a percentage
; of the change from one iteration to the other
(define (sqrt-prec a)
  (define (square-diff a b)
    (abs (- (square b) (square a))))

  (define (good-enough-prec? guess last-guess)
    (< (/
         (square-diff guess last-guess)
         guess)
       tolerance))

  (define (sqrt-iter-prec guess a last-guess)
    (if (good-enough-prec? guess last-guess)
      guess
      (sqrt-iter-prec (improve-guess guess a) a guess)))

  (sqrt-iter-prec 1.0 a a))

(sqrt-prec 2)


; cchapter 1.3.2
(define (factorial n)
  (cond
    ((< n 1) 0)
    ((= n 1) 1)
    (else (* n (factorial (- n 1))))))

(factorial 0)
(factorial 1)
(factorial 5)

(define (factorial-iter previous current count)
  (if (= count 0)
    previous
    (factorial-iter current (+ previous current) (- count 1))))


; exercise 1.9
; First process is recursive. the function's return arguments
; depend on the execution of itself.
; Seocnd process is iterative. The return arguments do not call
; the function itself.

; exercise 1.10
; values of the expressions:
; - 1024
; - 256
; - 512
; mathematical definitions
; - f -> 2^0
; - g -> 2^n
; - h-> 2^2n
; - k -> 5n^2
