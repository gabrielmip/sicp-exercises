(load "1A-overview-and-introduction-to-lisp.scm")

(define (naive-factorial n)
  (if (= n 1)
      1
      (* n (naive-factorial (- n 1)))))

(define (iter-factorial n current prod)
  (if (= current n)
    prod
    (iter-factorial n (+ current 1) (* (+ 1 current) prod))))

(iter-factorial 4 1 1)

; exercicio 1.9
; primeiro procedimento, assumindo a aplicação do decrescimento sem
; precisar expressar
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

; processo recursivo

; segundo procedimento
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
; processo linear

; -----------

; exercicio 1.10
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
1024
; 2^y

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2))) ; (A 1 2) -> 2^y
65536
; 2^(2^4)

; sobre os procedimentos
(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 2^n
(define (f n) (A 2 n)) ; 2^(2^n)

; -----------
; exemplo: counting change
(define (coin-value coin-id)
  (cond ((= coin-id 1) 1)
        ((= coin-id 2) 5)
        ((= coin-id 3) 10)
        ((= coin-id 4) 25)
        ((= coin-id 5) 50)))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (cc amount coin-id)
  (cond
    ((= amount 0) 1)
    ((< amount 0) 0)
    ((or (< coin-id 1) (> coin-id 5)) 0)
    (else (+ (cc (- amount (coin-value coin-id))
                           coin-id)
             (cc amount
                           (dec coin-id))))))

(define (count-change amount)
  (cc amount 5))

; exercicio 1.11
(define (rec-fn n)
  (cond ((< n 3) n)
        (else (+
                (rec-fn (- n 1))
                (* 2 (rec-fn (- n 2)))
                (* 3 (rec-fn (- n 3)))))))

(define (fn n1 n2 n3)
  (+ n1 (* 2 n2) (* 3 n3)))

(define (iter-fn-r n count n1 n2 n3)
  (cond ((= n count) (fn n1 n2 n3))
        (else (iter-fn-r n (inc count) (fn n1 n2 n3) n1 n2))))

(define (iter-fn n)
  (cond ((< n 3) n)
        (else (iter-fn-r n 3 2 1 0))))

; exercicio 1.12
; (define (pascal height row)
;   (cond ((= row 1) 1)
;         ((= row height) 1)
;         (else (+ (pascal (dec height) (dec row)) (pascal (dec height) row)))))
(define (pascal height row)
  (cond ((= row 1) 1)
        ((= row height) 1)
        (else (+
                (pascal (dec height) (dec row))
                (pascal (dec height) row)))))

(pascal 5 4)

; exercicio 1.13 TODO para quando tiver passado por indução
