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

; Problema 7 - alguno?
; Programar el predicado recursivo alguno? que verifique si al menos un par
; elementos correspondientes de dos listas cumplen con un predicado binario.
; Probar con:
; > (alguno? < ‘(9 2 10) ‘(6 7 8)) => #t
; > (alguno? (lambda (x y) (negative? (- x y))) ‘(5 8 10) ‘(4 5 6))
; => #f
; > (alguno? > ‘(3 1 15) ‘(6 7 8)) => #t

(define (alguno? op lista1 lista2)
    (if (op (car lista1) (car lista2))
        #t
        (if (null? (cdr lista1))
            #f
            (alguno? op (cdr lista1) (cdr lista2)))))

; Problema 9 - filtra
; Programar la función de orden superior filtra que sin utilizar recursividad
; explícita elimine de una matriz de números todos los elementos que NO cumplan
; una condición unaria que se le pase como argumento. No utilizar el primitivo
; filter.
; Probar con:
; > (filtra negative? '((1 -2 3 4)(-5 6 -7 -8))) => ((-2)(-5 -7 -8))
; > (filtra (lambda (x) (> x 5)) '((4 9)(1 2)(10 7))) => ((9)()(10 7))

(define (filtra-lista op lista)
    (if (null? lista)
        null
        (if (eq? (op (car lista)) #t)
            (append (list (car lista)) (filtra-lista op (cdr lista)))
            (filtra-lista op (cdr lista)))))

(define (filtra op matriz)
    (if (null? matriz)
        null
        (cons (filtra-lista op (car matriz)) (filtra op (cdr matriz)))))


