#lang racket
; Problema 1
; (unos '(0 0 0 0 1 1))
(define (unos input)
  (unos-aux input 0 (length input)))
(define (unos-aux lista pos length)
  (cond [(empty? lista) 0]
        [(= (car lista) 1) (- length pos)]
        [else (unos-aux (cdr lista) (+ pos 1) length)]))

; Problema 2
; (invierte '((0 1 0)(1 0 1)))
(define (invierte matrix)
  (cond [(empty? matrix) null]
        [(list? (car matrix)) (cons (invierte (car matrix)) (invierte (cdr matrix)))]
        [else (if (= (car matrix) 0) (cons 1 (invierte (cdr matrix))) (cons 0 (invierte (cdr matrix))))]))

; Problema 3
; (parcial 2 '((a01111111 (Jorge Perez) (100 100)) (a02222222 (Gloria Flores) (90)) (a03333333 (Ramiro Mendez) (90 60 90))))
(define (parcial index matrix)
  (cond [(empty? matrix) null]
        [(list? (car matrix)) (cons (parcial index (car matrix)) (parcial index (cdr matrix)))]
        [(list? (cddr matrix)) (cons (car matrix) (parcial-aux index (cddr matrix)))]))
(define (parcial-aux index califs)
  (if (> index (length (car califs))) (cons 'NO '()) (cons (list-ref (car califs) (- index 1)) '())))

; Problema 4
; (subarbol 'b '(a(b(c()())(d()()))(e()(f(g()())()))))
(define (subarbol id arbol)
  (cond [(empty? arbol) null]
        [(empty? (car arbol)) null]
        [(equal? id (car arbol)) arbol]
        [(not (null? (subarbol id (cadr arbol)))) (subarbol id (cadr arbol))]
        [(not (null? (subarbol id (caddr arbol)))) (subarbol id (caddr arbol))]
        [else null]))

; Problema 5
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