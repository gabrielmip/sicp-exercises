(cons-stream 1 the-empty-stream)

(stream-map (lambda (x) (* 2 x)) (cons-stream 1 the-empty-stream))

(stream-for-each (lambda (x) (* 2 x)) (display (cons-stream 1 the-empty-stream)))

(define (stream-take n s)
  (if (or (<= n 1) (stream-null? (stream-cdr s)))
    (stream-car s)
    (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

(define (stream-enumerate-interval lower higher)
  (if (> lower higher)
    the-empty-stream
    (cons-stream lower (stream-enumerate-interval (+ 1 lower) higher))))

(stream-take 5 (stream-enumerate-interval 10 40))

(define (stream-display s)
  (stream-for-each (lambda (x) (display x) (newline))
                   s))

(stream-display (stream-enumerate-interval 10 40))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ 1 n))))

(define integers (integers-starting-from 1))

(define (divisible? endo sor) (= (remainder endo sor) 0))
(define (no-sevens)
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-take 50 (no-sevens))

; muito legaaaaaaal
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             ; vai acumulando os predicados, filtrando por cada primo que veio
             ; antes dele
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (repeat x) (cons-stream x (repeat x)))

(define (add-streams s1 s2) (stream-map + s1 s2))


(define integers (cons-stream 1 (add-streams ones integers)))
; pra entender o integers substituindo as expressões
; (cons-stream 1
;              (add-streams ones
;                           (cons-stream 1 (add-streams ones
;                                                       (cons-stream 1 (add-streams ones integers))))))

; funcionaaaaaaaaaaaaaaaaaaaaaa!!!!!!!!!!!!!!!!!!!!!!
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams fibs (stream-cdr fibs)))))

(define (scale-stream factor stream)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream 2 double)))
; para entender o double{{{
; (cons-stream 1
;              (scale-stream
;                2 (cons-stream 1 (scale-stream
;                                   2 (cons-stream 1 (scale-stream
;                                                      2 (cons-stream 1 (scale-stream 2 double))))))))
;}}}

; usando stream infinito para gerar os chutes da raiz quadrada de um numero {{{
(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (display "coé") (sqrt-improve guess x))
                  guesses)))
  guesses)
; }}}

; minha aproximação para o pi {{{
(define (two-by-two n)
  (cons-stream (/ 1 n)
               (stream-map - (two-by-two (+ n 2)))))

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream)
                            (partial-sums stream))))

(define pi-approx (scale-stream 4 (partial-sums (two-by-two 1))))
; }}}

(stream-take 10 pi-approx)

(define summands (cons-stream 1 (add-stream )))

(stream-take 5 (sqrt-stream 2))

