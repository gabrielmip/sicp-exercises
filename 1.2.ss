(define (factorial n)
  (cond
    ((< n 1) 0)
    ((= n 1) 1)
    (else (* n (factorial (- n 1))))))

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


; section 1.2.1
; counting number of ways to change 1 dollar
(define (count-change amount)
  (count-change-rec amount 5))

(define (count-change-rec amount coin-kinds)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= coin-kinds 0)) 0)
        (else
          (+
            (count-change-rec amount (- coin-kinds 1))
            (count-change-rec
              (- amount (coin-value coin-kinds))
              coin-kinds)))))

(define (coin-value coin-kind)
  (cond
    ((= coin-kind 1) 1)
    ((= coin-kind 2) 5)
    ((= coin-kind 3) 10)
    ((= coin-kind 4) 25)
    ((= coin-kind 5) 50)))

; exercise 1.11 - recursive process
(define (f1-greater-n fac1 fac2 fac3)
  (+
    fac1
    (* 2 fac2)
    (* 3 fac3)))

(define (f1 n)
  (if (< n 3)
    n
    (f1-greater-n
      (f1 (- n 1))
      (f1 (- n 2))
      (f1 (- n 3)))))

; exercise 1.11 - iterative process
(define (f1-iter n)
  (f1-iter-rec 0 1 2 n))

(define (f1-iter-rec minus3 minus2 minus1 currentN)
  (if (< currentN 3)
    minus1
    (f1-iter-rec
      minus2
      minus1
      (f1-greater-n
        minus1
        minus2
        minus3)
      (- currentN 1))))

; exercise 1.12
(define (pascal-elem line column)
  (cond
    ((= line column 1) 1)
    ((and
       (= line 1)
       (not (= column 1)))
     0)
    (else
      (+
        (pascal-elem (- line 1) column)
        (pascal-elem (- line 1) (- column 1))))))

