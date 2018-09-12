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
