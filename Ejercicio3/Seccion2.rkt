#lang racket
; A01634310 | Diego Estrada Talamantes
; A00822649 | Alberto García Viegas

; Problema 6 - aplica-listas
; > (aplica-listas (list + - * /) '(1 2 3) '(4 5 6))            => ((5 7 9)(-3 -3 -3)(4 10 18)(1/4 2/5 1/2))
; > (aplica-listas (list cons list append) '((a b)) '((c d)))   => ((((a b) c d))(((a b)(c d)))((a b c d)))

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
; > (alguno? < '(9 2 10) '(6 7 8))                                  => #t
; > (alguno? (lambda (x y) (negative? (- x y))) '(5 8 10) '(4 5 6)) => #f
; > (alguno? > '(3 1 15) '(6 7 8))                                  => #t

(define (alguno? op lista1 lista2)
    (if (op (car lista1) (car lista2))
        #t
        (if (null? (cdr lista1))
            #f
            (alguno? op (cdr lista1) (cdr lista2)))))

; Problema 8 - verbosa
;(define vinc (verbosa (lambda (x) (+ x 1))))
(define (verbosa funcion)
  (lambda (valor) (display (string-append "Entrada = " (number->string valor) "\n" "Salida = " (number->string (funcion valor))))))
(define vinc (verbosa (lambda (x) (+ x 1))))

; Problema 9 - filtra
; > (filtra negative? '((1 -2 3 4)(-5 6 -7 -8)))        => ((-2)(-5 -7 -8))
; > (filtra (lambda (x) (> x 5)) '((4 9)(1 2)(10 7)))   => ((9)()(10 7))

(define filtra 
    (lambda (oper matriz)
        (map 
            (lambda (lista)
                (apply append 
                    (map 
                        (lambda (valor) 
                            (if (oper valor)
                                (list valor)
                                null)) 
                    lista))) 
        matriz)))

; Problema 10 - impares
;(impares '((1 2 3)(4 5 6)))
(define (impares matrix)
  (remove* '(()) (append-map (lambda (lista) (map (lambda (casilla) (if (odd? casilla) casilla null)) lista)) matrix)))

; Problema 11 - inserta
; > (inserta 1 '(1 2 3 4))        => (1 1 2 1 3 1 4 1)
; > (inserta 'a '(b (c) (d e)))   => (b a (c) a (d e) a)

(define inserta-valor 
    (lambda (op init valor lista)
        (if (null? lista)
            init
            (op (car lista) (op valor (inserta-valor op init valor (cdr lista)))))))

(define inserta (lambda (valor lista) (inserta-valor cons null valor lista)))

; Problema 12 - multifnc
;(multifnc '(sqr sqrt (lambda (v) (/ 1 v))) '(1 4 9))
(define ns (variable-reference->namespace (#%variable-reference)))
(define (string->procedure sym)
  (eval sym ns))
(define (multifnc operaciones valores)
  (map (lambda (op) (map (string->procedure op) valores)) operaciones))