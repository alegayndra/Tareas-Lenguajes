#lang racket
; A01634310 | Diego Estrada Talamantes
; A00822649 | Alberto García Viegas


; par-menor
(define (par-menor a b c d e)
  (cond [(> a b) ])
  )


; Implementar la función recursiva logaritmo que regrese el valor del logaritmo de y=1+x mediante el cálculo de n términos
(define (logaritmo x n)
  (if (= n 0)
      0
      (if (= (modulo n 2) 0)
          (+ (* -1 (/ (expt x n) n)) (logaritmo x (- n 1)))
          (+ (/ (expt x n) n) (logaritmo x (- n 1)))
          )
      )
  )

; Implementar la función recursiva secuencias que despliegue N secuencias alternadas de los enteros del 1 al M
(define (secuencias N M)
  (if (= N 0) null
      (if (= N -2)
          (if (= M 0) '() (append (secuencias -2 (- M 1)) (cons M '())))
          (if (= N -1)
              (if (= M 0) '() (append (cons M '()) (secuencias -1 (- M 1))))
              (if (= (modulo N 2) 0)
                  (append (secuencias (- N 1) M) (cons (secuencias -1 M) '()))
                  (append (secuencias (- N 1) M) (cons (secuencias -2 M) '()))
                  )
              )
          )
      )
  )

; repite: Dada una lista de enteros no negativos, regrese una lista donde cada valor se repita el número de veces que representa.
; test01: (repite '(0 1 2 3))
; test02: (repite '(4 0 3 0 2))
(define (repite lista)
  (if (null? lista) null
      (append (repite-aux (car lista) (car lista)) (repite (cdr lista)))
      )
  )
(define (repite-aux base num)
  (if (= num 0) null
      (cons base (repite-aux base (- num 1)))
      )
  )

; contadores: que a partir de una lista de posiciones, dadas como enteros positivos, cree una lista de contadores
; donde cada valor en la lista represente la cantidad de veces que aparece esa posición en la lista de posiciones.
; test03: (contadores '(6 8 3 6 6 1))
(define (contadores lista)
  (if (null? lista) null
      (cons (contadores-aux lista (car lista)) (contadores (cdr lista)))
      )
  )
(define (contadores-aux lista pos)
  (if (= (car lista) pos)
      (+ 1 (contadores-aux (cdr lista) pos))
      (contadores-aux (cdr lista) pos)
      )
  )

; enteros:  que regrese la cantidad total de enteros que se encuentre en una secuencia arbitraria de listas planas que contienen enteros y símbolos.
; test03: (enteros '(1 a) '(2 3) '(b c) '(9 d))
(define (enteros . lista)
  (cond [(null? lista) 0]
        [(list? lista) (+ (enteros (car lista)) (enteros (cdr lista)))]
        [(number? lista) 1]
        [else 0]
        )
  )

; forma: (lista plana, dos enteros positivos N y M) lista con N sublistas que contienen M elementos cada una.
; Si la lista plana no contiene NxM elementos, los elementos faltantes deberán aparecer como guiónes (-) y si la lista plana
; tiene más de NxM elementos

(define (forma lista N M))




; enumera:  que dada una lista posiblemente imbricada, regresa una lista con la misma forma,
; pero que en lugar de cada valor original regrese un número que indique su
; profundidad y su posición en la (sub)lista donde se encuentra.
; test02: (enumera '(3 (b (c 2 (d)) a) 1))
(define (enumera lista)
  (enumera-aux lista 1 1))
(define (enumera-aux lista prof pos)
  (if (null? list) null
      (if (list? (car lista))
          (cons (enumera-aux (car lista) prof 1) (enumera-aux (cdr lista) prof 1))
          (+ prof (/ pos 10))
          )
      )
  )