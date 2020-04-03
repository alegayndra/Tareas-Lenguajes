#lang racket
;(aplica-listas (list + - * /) '(1 2 3) '(4 5 6))
(define (aplica-listas operations list-a list-b)
  (if (not (empty? operations)) (cons (aplica-listas-aux (car operations) list-a list-b) (aplica-listas (cdr operations) list-a list-b)) '()))
(define (aplica-listas-aux operation list-a list-b)
  (cond [(null? list-a) '()]
        [else (cons (operation (car list-a) (car list-b)) (aplica-listas-aux operation (cdr list-a) (cdr list-b)))])
  )

;(define vinc (verbosa (lambda (x) (+ x 1))))
(define (verbosa funcion)
  (lambda (valor) (display (string-append "Entrada = " (number->string valor) "\n" "Salida = " (number->string (funcion valor))))))
(define vinc (verbosa (lambda (x) (+ x 1))))

;(impares '((1 2 3)(4 5 6)))
(define (impares matrix)
  (remove* '(()) (append-map (lambda (lista) (map (lambda (casilla) (if (odd? casilla) casilla null)) lista)) matrix)))

;(multifnc '(sqr sqrt (lambda (v) (/ 1 v))) '(1 4 9))
(define ns (variable-reference->namespace (#%variable-reference)))
(define (string->procedure sym)
  (eval sym ns))
(define (multifnc operaciones valores)
  (map (lambda (op) (map (string->procedure op) valores)) operaciones))