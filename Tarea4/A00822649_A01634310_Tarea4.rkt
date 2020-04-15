#lang racket
; A01634310 | Diego Estrada Talamantes
; A00       | Alberto Garcia Viegas



; Problema 1 - Árboles Binarios ------------------------------------------------------------------

; Arbol para pruebas
(define AB '(8 (5 (2 () ())
                  (7 () ()))
               (9 ()
                  (15 (11 () ())
                      () ))))

; 1.a) Define si el formato es congruente a arboles binarios
; (arbol-binario? '()) => #t
; (arbol-binario? AB) => #t
; (arbol-binario? '(a (b (() ()) ()) (d () ()))) => #f
(define (arbol-binario? arbol)
  (cond [(empty? arbol) true]
        [(= (length arbol) 3) (and (arbol-binario? (cadr arbol)) (arbol-binario? (caddr arbol)))]
        [else false]))

; 1.b) Regresa los numeros menores al indicado en un arbol
; (obten-menores AB 2) => ()
; (obten-menores AB 8) => (5 3 7)
; (obten-menores AB 20) => (8 5 2 7 9 15 11)
(define (obten-menores arbol target)
  (cond [(empty? arbol) '()]
        [(< (car arbol) target) (flatten (list (car arbol) (obten-menores (cadr arbol) target) (obten-menores (caddr arbol) target)))]
        [else (obten-menores (cadr arbol) target)]))

; 1.c) Regresa los valores de la rama más larga de un arbol
; (rama+larga '()) => ()
; (rama+larga AB) => (8 9 15 11)
; (rama+larga '(a (b (c () ()) ()) (d (e (f (g (h () ()) ()) ()) ()) ()))) => (a b c)
(define (rama+larga arbol)
  (cond [(empty? arbol) '()]
        [(>= (length (rama+larga (cadr arbol))) (length (rama+larga (caddr arbol)))) (cons (car arbol) (rama+larga (cadr arbol)))]
        [else (cons (car arbol) (rama+larga (caddr arbol)))]))



; Problema 2 - Grafo Dirigido ------------------------------------------------------------------

; Grafo para pruebas
(define g
  '((A ((B 2) (D 10)))
    (B ((C 9) (E 5)))
    (C ((A 12) (D 6)))
    (D ((E 7)))
    (E ((C 3)))
    ))

; 2.1) Regresa los nodos dostino de un nodo en particular del grafo
; (nodos-destino g 'A) => (B D)
; (nodos-destino g 'D) => (E)
; (nodos-destino g 'F) => ()
(define (nodos-destino grafo target)
  (cond [(empty? grafo) '()]
        [else
         (if (equal? (caar grafo) target)
             (flatten (nodos-destino-aux (cadar grafo)))
             (nodos-destino (cdr grafo) target))
         ]))
(define (nodos-destino-aux conexiones)
  (cond [(empty? conexiones) '()]
        [else (flatten (list (caar conexiones) (nodos-destino-aux (cdr conexiones))))]))

; 2.2) Regresa los nodos origen que tiene un nodo en particular del grafo
; (nodos-origen g 'A) => (C)
; (nodos-origen g 'C) => (B E)
; (nodos-origen g 'F) => ()
(define (nodos-origen grafo target)
  (cond [(empty? grafo) '()]
        [(list? (car grafo)) (flatten (list (nodos-origen-aux (car grafo) target) (nodos-origen (cdr grafo) target)))]
        [else '()]))
(define (nodos-origen-aux nodo target)
  (cond [(member target (flatten (cdr nodo))) (car nodo)]
        [else '()]))

; 2.3) Eliminar un arco en especifico del grafo otorgado
; (elimina-arco g 'B 'C) => ((A ((B 2) (D 10))) (B ((E 5))) (C ((A 12) (D 6))) (D ((E 7)))(E ((C 3)))))
; (elimina-arco g 'B 'D) => ((A ((B 2) (D 10))) (B ((C 9) (E 5))) (C ((A 12) (D 6))) (D ((E 7)))(E ((C 3)))))
; (elimina-arco g 'F 'C) => ((A ((B 2) (D 10))) (B ((C 9) (E 5))) (C ((A 12) (D 6))) (D ((E 7)))(E ((C 3)))))
(define (elimina-arco grafo orig dest)
  (cond [(empty? grafo) '()]
        [else
         (if (equal? (caar grafo) orig)
             (cons (elimina-arco-aux (car grafo) dest) (elimina-arco (cdr grafo) orig dest))
             (cons (car grafo) (elimina-arco (cdr grafo) orig dest)))
         ]))
(define (elimina-arco-aux nodo dest)
  (cons (car nodo) (list (elimina-arco-aux-aux (cadr nodo) dest))))
(define (elimina-arco-aux-aux conexiones dest)
  (cond [(empty? conexiones) '()]
        [(equal? (caar conexiones) dest) (cdr conexiones)]
        [else (cons (car conexiones) (elimina-arco-aux-aux (cdr conexiones) dest))]))



; Problema 3 - Funciones de orden superior ------------------------------------------------------------------

; 3.1) Contar la cantidad de ceros dentro de una matriz
; (cuenta-ceros '())                            ; => 0
; (cuenta-ceros '((0 1)(2 3)))                  ; => 1
; (cuenta-ceros '((4 0 3 1)(5 1 2 1)(6 0 1 1))) ; => 2

(define cuenta-ceros-aux
    (lambda (matriz)
        (apply append
            (map
                (lambda (lista)
                    (map
                        (lambda (valor)
                            (if (= valor 0) 1 0))
                    lista))
            matriz))))

(define cuenta-ceros
    (lambda (matriz)
        (if (null? matriz) 
            0
            (apply + (cuenta-ceros-aux matriz)))))


; (cuenta-ceros '())                            ; => 0
; (cuenta-ceros '((0 1)(2 3)))                  ; => 1
; (cuenta-ceros '((4 0 3 1)(5 1 2 1)(6 0 1 1))) ; => 2

; 3.2) Regresar lista con el valor menor y mayor dentro de una matriz
(define aplanar-matriz
    (lambda (matriz)
        (apply append matriz)))

(define minmax-aux
    (lambda (oper valor lista) 
        (cond   ((null? lista) valor)
                ((oper valor (car lista))   (minmax-aux oper valor       (cdr lista)))
                (else                       (minmax-aux oper (car lista) (cdr lista)))
            )))

(define minmax
    (lambda (matriz) (cons (minmax-aux < (car (aplanar-matriz matriz)) (cdr (aplanar-matriz matriz))) (cons (minmax-aux > (car (aplanar-matriz matriz)) (cdr (aplanar-matriz matriz))) null))))

; Casos de pruebas
; (minmax '((2)))                           ; => (2 2)
; (minmax '((0 1)(2 3)))                    ; => (0 3)
; (minmax '((4 0 -3 1)(5 -1 2 1)(6 0 1 1))) ; => (-3 6)

; 3.3) Multiplicación de matrices
; (multmat '((1 2 3)(0 2 1)) '((4 0 3 1)(5 1 2 1)(6 0 1 1))) => ((32 2 10 6)(16 2 5 3))

(define (conseguir-val-mat-col lista col)
    (if (= col 0)
        (car lista)
        (conseguir-val-mat-col (cdr lista) (sub1 col))))

(define (conseguir-val-mat-ren mat reng col)
    (if (= reng 0)
        (conseguir-val-mat-col (car mat) col)
        (conseguir-val-mat-ren (cdr mat) (sub1 reng) col)))

(define (multiplicar-casilla lista mat reng col )
    (if (null? lista)
        0
        (+ (* (car lista) (conseguir-val-mat-ren mat reng col)) (multiplicar-casilla (cdr lista) mat (add1 reng) col))))

(define (multiplicar-renglon lista mat col)
    (if (= col (add1 (length lista)))
        null
        (cons (multiplicar-casilla lista mat 0 col) (multiplicar-renglon lista mat (add1 col)))))

(define (multmat mat1 mat2)
    (if (null? mat1)
        mat1
        (cons (multiplicar-renglon (car mat1) mat2 0) (multmat (cdr mat1) mat2))))

(multmat '((1 2 3)(0 2 1)) '((4 0 3 1)(5 1 2 1)(6 0 1 1)))