#lang racket
; A00822649 | Alberto García Viegas
; A01634310 | Diego Estrada Talamantes

; Programar la función recursiva multiplica que regresa la lista que resulta de multiplicar un número por los elementos de una lista.
(define (multiplica num lista)
  (if (null? lista)
      lista
      (cons (* (car lista) num)
            (multiplica num (cdr lista))
            )
      )
  )

; Programar la función recursiva entero que regresa el número entero representado por una lista de dígitos.
(define (entero lista)
  (if (null? lista)
      0
      (if (>= (car lista) 0)
          (+ (* (car lista) (expt 10 (- (length lista) 1))) (entero (cdr lista)))
          (- (* (car lista) (expt 10 (- (length lista) 1))) (entero (cdr lista)))
          )
      )
  )

; Programar la función recursiva intercala que regresa la lista resultante de intercalar los elementos de dos listas hasta que alguna de las listas se acabe.
(define (intercala lista_a lista_b)
  (if (or (null? lista_a) (null? lista_b))
      null
      (cons (car lista_a) (cons (car lista_b) (intercala (cdr lista_a) (cdr lista_b))))
      )
  )

; Programar el predicado recursivo profundidad? que determine si en una lista (primer argumento) existe algún elemento a una profundidad dada (segundo argumento).
(define (profundidad? lista prof)
  (if (null? lista)
      (= prof 1)
      (if (list? (car lista))
          (or (profundidad? (car lista) (- prof 1)) (profundidad? (cdr lista) prof))
          (or (= prof 1) (profundidad? (cdr lista) prof))
          )
      )
  )

; Programar la función recursiva tabla que cree una tabla con N renglones y M columnas llena con el mismo valor V.
(define (tabla N M V)
  (if (= N 0)
      (if (= M 0) '() (cons V (tabla 0 (- M 1) V)))
      (if (= N 1)
          (if (= M 0) null (cons (tabla 0 M V) '()))
          (cons (car (tabla 1 M V)) (tabla (- N 1) M V))
          )
      )
  )


; Programar la función recursiva concatena que concatene todas las sublistas de una lista posiblemente imbricada.
(define (concatena lista)
  (if (null? lista)
      null
      (if (list? (car lista))
          (append (car lista) (concatena (cdr lista)))
          (concatena (cdr lista))
          )
      )
  )
