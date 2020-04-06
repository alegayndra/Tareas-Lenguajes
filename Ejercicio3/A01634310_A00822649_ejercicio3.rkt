#lang racket
; A01634310 | Diego Estrada Talamantes
; A00822649 | Alberto García Viegas

; Sección 1 -------------------------------------------------------------------------------------------------------------------------------------------

; Problema 1 - unos
; (unos '(0 0 0 0 1 1))
(define (unos input)
  (unos-aux input 0 (length input)))
(define (unos-aux lista pos length)
  (cond [(empty? lista) 0]
        [(= (car lista) 1) (- length pos)]
        [else (unos-aux (cdr lista) (+ pos 1) length)]))

; Problema 2 - invierte
; (invierte '((0 1 0)(1 0 1)))
(define (invierte matrix)
  (cond [(empty? matrix) null]
        [(list? (car matrix)) (cons (invierte (car matrix)) (invierte (cdr matrix)))]
        [else (if (= (car matrix) 0) (cons 1 (invierte (cdr matrix))) (cons 0 (invierte (cdr matrix))))]))

; Problema 3 - parcial
; (parcial 2 '((a01111111 (Jorge Perez) (100 100)) (a02222222 (Gloria Flores) (90)) (a03333333 (Ramiro Mendez) (90 60 90))))
(define (parcial index matrix)
  (cond [(empty? matrix) null]
        [(list? (car matrix)) (cons (parcial index (car matrix)) (parcial index (cdr matrix)))]
        [(list? (cddr matrix)) (cons (car matrix) (parcial-aux index (cddr matrix)))]))
(define (parcial-aux index califs)
  (if (> index (length (car califs))) (cons 'NO '()) (cons (list-ref (car califs) (- index 1)) '())))

; Problema 4 - subarbol
; (subarbol 'b '(a(b(c()())(d()()))(e()(f(g()())()))))
(define (subarbol id arbol)
  (cond [(empty? arbol) null]
        [(empty? (car arbol)) null]
        [(equal? id (car arbol)) arbol]
        [(not (null? (subarbol id (cadr arbol)))) (subarbol id (cadr arbol))]
        [(not (null? (subarbol id (caddr arbol)))) (subarbol id (caddr arbol))]
        [else null]))

; Problema 5 - adyacentes
; (adyacentes 3 '((1 2 3 4)((1 2)(1 4)(2 3)(2 4)(3 4))))
; (adyacentes 'a '((a b c)((a b)(c b)(c a))))
(define (adyacentes target lista)
  (concat-nodos target (cadr lista)))
(define (concat-nodos target lista)
  (cond [(empty? lista) '()]
        [else (append (adyacentes-aux target (car lista)) (concat-nodos target (cdr lista)))]
        ))
(define (adyacentes-aux target lista)
  (cond [(not (list? lista)) '()]
        [else (if (member target lista) (remove target lista) '())]))

; Sección 2 -------------------------------------------------------------------------------------------------------------------------------------------

; Problema 6 - aplica-listas
; > (aplica-listas (list + - * /) '(1 2 3) '(4 5 6))            ;=> ((5 7 9)(-3 -3 -3)(4 10 18)(1/4 2/5 1/2))
; > (aplica-listas (list cons list append) '((a b)) '((c d)))   ;=> ((((a b) c d))(((a b)(c d)))((a b c d)))

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
; > (alguno? < '(9 2 10) '(6 7 8))                                  ;=> #t
; > (alguno? (lambda (x y) (negative? (- x y))) '(5 8 10) '(4 5 6)) ;=> #f
; > (alguno? > '(3 1 15) '(6 7 8))                                  ;=> #t

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
; > (filtra negative? '((1 -2 3 4)(-5 6 -7 -8)))        ;=> ((-2)(-5 -7 -8))
; > (filtra (lambda (x) (> x 5)) '((4 9)(1 2)(10 7)))   ;=> ((9)()(10 7))

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
; > (impares '((1 2 3)(4 5 6)))  ;=> (1 3 5)
; > (impares '((2 4)(2 1)(3 2))) ;=> (1 3)

(define (impares matrix)
  (remove* '(()) (append-map (lambda (lista) (map (lambda (casilla) (if (odd? casilla) casilla null)) lista)) matrix)))

; Problema 11 - inserta
; > (inserta 1 '(1 2 3 4))        ;=> (1 1 2 1 3 1 4 1)
; > (inserta 'a '(b (c) (d e)))   ;=> (b a (c) a (d e) a)

(define inserta-valor 
    (lambda (op init valor lista)
        (if (null? lista)
            init
            (op (car lista) (op valor (inserta-valor op init valor (cdr lista)))))))

(define inserta (lambda (valor lista) (inserta-valor cons null valor lista)))

; Problema 12 - multifnc
; > (multifnc '(sqr sqrt (lambda (v) (/ 1 v))) '(1 4 9))  ;=> ((1 16 81)(1 2 3)(1 1/4 1/9))
; > (multifnc '(car cdr list) '((1 2)(a b)))              ;=> ((1 a)((2)(b))(((1 2))((a b))))

(define ns (variable-reference->namespace (#%variable-reference)))
(define (string->procedure sym)
  (eval sym ns))
(define (multifnc operaciones valores)
  (map (lambda (op) (map (string->procedure op) valores)) operaciones))