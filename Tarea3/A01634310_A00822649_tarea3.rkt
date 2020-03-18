#lang racket
; A01634310 | Diego Estrada Talamantes
; A00822649 | Alberto García Viegas

; Ejercicio 1
; par-menor:         se encarga de empezar la recursividad con la funcion auxiliar
; par-menor-aux:     se encarga de dejar los números más pequeños a la izquierda y recorrer los numeros faltantes para comparar todos
; par-menor-display: se encarga de desplegar los números
(define (par-menor a b c d e)
  (par-menor-aux a b c d e 0))

(define (par-menor-aux a b c d e cont)
  (if (< cont 3)
      (cond ((and (< a c) (< b c)) (par-menor-aux a b d e c (+ cont 1)))
            ((and (< a c) (< c b)) (par-menor-aux a c d e b (+ cont 1)))
            ((and (< c a) (< b c)) (par-menor-aux c b d e a (+ cont 1))))
      (par-menor-display a b)))

(define (par-menor-display a b)
  (display a)
  (printf "-")
  (display b))

; Ejercicio 2
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

; Ejercicio 3
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

; Ejercicio 5
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

; Ejercicio 6
; contadores: que a partir de una lista de posiciones, dadas como enteros positivos, cree una lista de contadores
; donde cada valor en la lista represente la cantidad de veces que aparece esa posición en la lista de posiciones.
; test03: (contadores '(6 8 3 6 6 1))
(define (contadores lista) (reverse (contadores-shido lista (max-num lista 0))))
(define (contadores-shido lista pos)
  (if (= pos 0) '()
      (cons (contadores-aux lista pos) (contadores-shido lista (- pos 1)))
      )
  )
(define (contadores-aux lista pos)
  (if (null? lista) 0
      (if (= (car lista) pos)
          (+ 1 (contadores-aux (cdr lista) pos))
          (contadores-aux (cdr lista) pos)
          )
      )
  )
(define (max-num lista maxim)
  (if (null? lista) maxim
      (if (> (car lista) maxim) (max-num (cdr lista) (car lista)) (max-num (cdr lista) maxim))))
  

; Ejercicio 7
; enteros:  que regrese la cantidad total de enteros que se encuentre en una secuencia arbitraria de listas planas que contienen enteros y símbolos.
; test03: (enteros '(1 a) '(2 3) '(b c) '(9 d))   => 4
(define (enteros . lista)
  (if (empty? (car lista)) 0
      (if (number? (caar lista))
          (+ 1 (enteros (flatten (cdr (flatten lista)))))
          (enteros (flatten (cdr (flatten lista))))
          )
      )
  )

; Ejercicio 8
; forma: (lista plana, dos enteros positivos N y M) lista con N sublistas que contienen M elementos cada una.
; Si la lista plana no contiene NxM elementos, los elementos faltantes deberán aparecer como guiónes (-) y si la lista plana
; tiene más de NxM elementos
(define (forma lista N M)
  (if (zero? N)
    '()
    (cons (forma-aux lista M) (forma (if (> (length lista) M) (drop lista M) (drop lista (length lista))) (- N 1) M))))

(define (forma-aux lista M)
  (if (zero? M)
    '()
    (if (null? lista)
      (forma-aux-null M)
      (cons (car lista) (forma-aux (cdr lista) (- M 1))))))

(define (forma-aux-null M)
  (if (zero? M)
    '()
    (cons '- (forma-aux-null (- M 1)))))
    
; Ejercicio 9
; enumera:  que dada una lista posiblemente imbricada, regresa una lista con la misma forma,
; pero que en lugar de cada valor original regrese un número que indique su
; profundidad y su posición en la (sub)lista donde se encuentra.
; test02: (enumera '(3 (b (c 2 (d)) a) 1))
(define (enumera lista)
  (enumera-aux lista 1 1))

(define (enumera-aux lista prof pos)
  (if (null? lista) '()
      (if (list? (car lista))
          (cons (enumera-aux (car lista) (+ prof 1) 1) (enumera-aux (cdr lista) prof (+ pos 1)))
          (cons (string-append (itos prof) "." (itos pos)) (enumera-aux (cdr lista) prof (+ pos 1)))
          )
      )
  )

(define (itos num)
  (if (< num 10)
    (itos-aux num)
    (string-append (itos (truncate (/ num 10))) (itos-aux (remainder num 10)))))

(define (itos-aux num)
  (cond ((= num 0) "0")
        ((= num 1) "1")
        ((= num 2) "2")
        ((= num 3) "3")
        ((= num 4) "4")
        ((= num 5) "5")
        ((= num 6) "6")
        ((= num 7) "7")
        ((= num 8) "8")
        ((= num 9) "9")))

