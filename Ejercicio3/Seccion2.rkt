#lang racket
; A01634310 | Diego Estrada Talamantes
; A00822649 | Alberto García Viegas

; Problema 6 - aplica-listas
; Programar la función recursiva aplica-listas que aplique una lista de funciones
; binarias a cada elemento correspondiente en dos listas del mismo tamaño para
; obtener una lista de sublistas con los resultados de cada operador.
; Probar con:
; > (aplica-listas (list + - * /) ‘(1 2 3) ‘(4 5 6))
; => ((5 7 9)(-3 -3 -3)(4 10 18)(1/4 2/5 1/2))
; > (aplica-listas (list cons list append) ‘((a b)) ‘((c d)))
; => ((((a b) c d))(((a b)(c d)))((a b c d)))

(define (aplica-operador op lista1 lista2)
    (if (null? lista1)
        null
        (cons (op (car lista1) (car lista2)) (aplica-operador op (cdr lista1) (cdr lista2)))))

(define (aplica-listas lista-op lista1 lista2)
    (cons (aplica-operador (car lista-op) lista1 lista2) 
        (if (null? (cdr lista-op))
            null
            (list (aplica-listas (cdr lista-op) lista1 lista2)))))