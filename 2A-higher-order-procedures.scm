(load "1B-Procedures-and-Processes-Substitution-Model.scm")

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (identity x) x)
(define (inc x) (+ 1 x))
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2))
          (lambda (x) (+ x dx))
          b)
     dx))

(integral cube 0 1 0.00001)

; exercicio 1.29
(define (integral-simpson f a b n)
  (define (factor x)
    (cond ((= x 0) 1)
          ((= x n) 1)
          ((= (modulo x 2) 0) 2)
          (else 4)))
  (define (term i)
    (* (factor i)
       (f (+ a (* i (/ (- b a) n))))))
  (* (/ (- b a) n 3)
     (sum term 0 inc n)))

(integral-simpson cube 0 1 100)

; exercicio 1.30
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; exercicio 1:31
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 6)

(define (double x) (* x 2))

(define (iinc x) (inc (inc x)))

(define (pi-factor x)
    (/ (* (double x)
          (iinc (double x)))
       (square (inc (double x)))))

(define (pi n)
  (* (product pi-factor 1 inc n)
     4))

(exact->inexact (pi 196))

(define (iter-pi n)
  (define (iter a result)
    (if (> a n)
      (* 4 result)
      (iter (inc a) (* result (pi-factor a)))))
  (iter 1 1))

(exact->inexact (iter-pi 196))

; exercicio 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; exercicio 1.33
(define (filter-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (if (filter a)
              (combiner result (term a))
              result))))
  (iter a null-value))

; a.
(define (prime? x)
  (define (iter i)
    (if (>= i x)
      #t
      (if (= (remainder x i) 0)
        #f
        (iter (inc i)))))
  (iter 2))

(define (prime-sum a b)
  (filter-accumulate + 0 square a inc b prime?))

(prime-sum 1 5)

; b.
(define (product-prime-relative x)
  (filter-accumulate *
                     1
                     identity
                     1
                     inc
                     (- x 1)
                     (lambda (y) (= (gcd x y) 1))))
(product-prime-relative 10)

; constructing procedures with lambda/let
(define (f x y)
  ((lambda (a b)
     85
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
; equivalent to
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
; the use of lambda for local variables are mind bending :D
