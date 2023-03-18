
(define ten 10)

(define (square x) (* x x))
(define square (lambda (x) (* x x))) ; equivalent to the above

(square 10)

; ---

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

; ---

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(abs 1)
(abs 0)
(abs -1)

; ---

(define a 3)
(define b (+ a 1))

(if (and (> b a) (< b (* a b)))
  b
  a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

; exercicio 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; exercicio 1.3
(define (is-smaller x y z)
  (and (< x y) (< x z)))

(define (larger-sum-of-squares x y z)
  (cond ((is-smaller x y z) (sum-of-squares y z))
        ((is-smaller y x z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

(larger-sum-of-squares 2 3 4)
(larger-sum-of-squares 2 4 3)
(larger-sum-of-squares 4 3 2)

; exercicio 1.4
; Como eu desejo negar o valor negativo de b caso ele seja negativo,
; eu defino o operador a ser usado na operaçõa de forma condicional
; ao sinal de b. Se ele é negativo, eu aplico a subtração. Caso contrário,
; a adição.

; exercicio 1.5
; interpretadores com avaliação de ordem aplicativa retornam 0. Na avaliação aplicativa,
; aplica-se o operador logo após a substituição dos argumentos.
; (test 0 (p)) -> (test 0 (p)) -> (if (= 0 0) 0 (p)) -> 0
; No caso de avaliação em ordem normal, a substituição se daria até que se chegasse
; em valores primitivos, mas a definição de p é recursiva sem caso base. Ela não termina.

(define (p) (p))
(define (test x y)
  (if (= x 0) x y))
; (test 0 (p)) ; deu pau no mit-scheme

; --- Square roots by newton's method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.0001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; exercicio 1.6
; Como new-if é uma função e não uma forma especial, seus operandos tem
; que ser avaliados antes da aplicação do operador. Seu segundo operando
; é recursivo, causando o não término da chamada para new-if.

; exercicio 1.7
; exemplo de número pequeno:
(sqrt 0.001) ; o resultado é 0.03274 e o esperado é 0.03162
; exemplo para número grande:
; (sqrt 100000000000000000000) ; não termina
; reescrita do good-enough?

(define (sqrt-iter guess x old-guess)
  (if (good-enough? guess old-guess)
    guess
    (sqrt-iter (improve guess x) x guess)))

(define (good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess)) old-guess)
     0.0000001))

(define (sqrt x)
  (sqrt-iter 1 x x))

(sqrt 0.001) ; o resultado é .03162277660168379 e o esperado é 0.0316227766017
             ; bem melhor para números menores

; exercicio 1.8
(define (cbrt x)
  (define (improve guess)
    (/ (+ (/ x (* guess guess))
          (* guess 2))
       3))
  (define (cbrt-iter guess old-guess)
    (if (good-enough? guess old-guess)
      guess
      (cbrt-iter (improve guess) guess)))
  (cbrt-iter 1 x))

(exact->inexact (cbrt 27))
(exact->inexact (cbrt 144))
(exact->inexact (cbrt 64))
