; rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define make-rat cons)
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (list))

(print-rat (make-rat 1 2))

; reduzindo as razões produzidas para os menores termos
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(make-rat 6 9)

; normalizando os sinais
(define (sign x)
  (if (< x 0) -1 1))

(define (rat-sign n d)
  (* (sign d) (sign n)))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
        (s (rat-sign n d)))
    (cons (* s (/ (abs n) g)) (/ (abs d) g))))

(make-rat 6 -9)

; exercicio 2.2 line segments
(define (make-segment start end) (cons start end))
(define start-segment car)
(define end-segment cdr)

(define (make-point x y) (cons x y))
(define x-point car)
(define y-point cdr)

(define (average x y) (/ (+ x y) 2))

(define (midpoint seg)
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(midpoint (make-segment
            (make-point 1 1)
            (make-point 3 4)))

; exercicio 2.3
; representação ou argumentos alternativos
; para o construtor: base (segmento) + altura
(define (make-rect top-left top-right bottom-right bottom-left)
  (cons (make-segment top-left top-right)
        (make-segment bottom-left bottom-right)))

(define rect-top car)
(define rect-bottom cdr)
(define (rect-left r)
  (make-segment (start-segment (rect-top r))
                (start-segment (rect-bottom r))))
(define (rect-right r)
  (make-segment (end-segment (rect-top r))
                (end-segment (rect-bottom r))))

(define (rect-top-left r)
  (start-segment (rect-top r)))

(define (rect-top-right r)
  (end-segment (rect-top)))

(define (rect-bottom-left r)
  (start-segment (rect-bottom r)))

(define (rect-bottom-right r)
  (end-segment (rect-bottom r)))

(define (segment-length seg)
  (let ((s (start-segment seg))
        (e (end-segment seg)))
    (sqrt (+ (square (- (x-point e) (x-point s)))
             (square (- (y-point e) (y-point s)))))))

(segment-length (make-segment
                  (make-point 1 1)
                  (make-point 3 4)))

(define (rect-perimeter r)
  (+ (segment-length (rect-top r))
     (segment-length (rect-left r))
     (segment-length (rect-right r))
     (segment-length (rect-bottom r))))

(define r (make-rect (make-point 1 4)
           (make-point 5 4)
           (make-point 5 1)
           (make-point 1 1)))

(rect-perimeter r)

(define (rect-area r)
  (* (segment-length (rect-top r))
     (segment-length (rect-left r))))

(rect-area r)
