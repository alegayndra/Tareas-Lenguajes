#lang racket
; A00822649 | Alberto García Viegas
; A01634310 | Diego Estrada Talamantes

; mayor: calcula el numero mayor de entre cuatro
(define [mayor a b c d]
  (if (> a b)
      (if (> a c)
          (if (> a d) a d)
          (if (> c d) c d))
      (if (> b c)
          (if (> b d) b d)
          (if (> c d) c d))))

; paronon: indica si dados cuatro argumentos, hay más pares o más nones
(define [paronon a b c d]
  (if (= (modulo a 2) 0)
      (if (= (modulo b 2) 0)
          (if (= (modulo c 2) 0)
              'pares
              (if (= (modulo d 2) 0)
                  'pares
                  'igual))
          (if (= (modulo c 2) 0)
              (if (= (modulo d 2) 0)
                  'pares
                  'igual)
              (if (= (modulo d 2) 0)
                  'igual
                  'nones)))
      (if (= (modulo b 2) 0)
          (if (= (modulo c 2) 0)
              (if (= (modulo d 2) 0)
                  'pares
                  'igual)
              (if (= (modulo d 2) 0)
                  'igual
                  'pares))
          (if (= (modulo c 2) 0)
              (if (= (modulo d 2) 0)
                  'igual
                  'nones)
              'nones))))

; Funcion recursiva para calcular serie
(define (serie n)
  (if (<= n 1) 3
      (+ (+ 1 (/ 2 n)) (serie (- n 1)))))

; Regresar n-esimo elemento de fibonacci ampliado
(define (fibo3 n)
  (if (<= n 3) 1
      (+ (+ (fibo3 (- n 1)) (fibo3 (- n 2))) (fibo3 (- n 3)))))

; Regresar n-esimo elemento de fibonacci ampliado con terminal
(define (fibo3t n)
  (fibo3t-aux n 1 1 1))

(define (fibo3t-aux n a b c)
  (cond [(= n 1) 1]
        [(= n 2) 1]
        [(= n 3) c]
        [else (fibo3t-aux (- n 1) b c (+ a b c))]))