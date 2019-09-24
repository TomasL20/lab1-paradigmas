#lang racket
;
;
;
(define (personaje posicion orientacion vida identidad)
  (if (and (string? identidad) (number? (car posicion)) (number? (cdr posicion)) (number? orientacion) (number? vida))
      (list posicion orientacion vida identidad)
      null
      )
  )
;
;
;
(define (personaje? pj)
  (if (list? pj)
      (if (and (pair? (car pj)) (number? (cadr pj)) (number? (caddr pj)) (string? (cadddr pj));los elementos de la lista deben cumplir el tipo de dato
               (number? (car (car pj))) (number? (cdr (car pj))) ;las coordenadas deben ser números
               (and (<= (caddr pj) 100) (> (caddr pj) 0))) ; el pj debe tener vida
          #t
          #f
          )
      #f
      )
  )
;
;
;
(define (getPosicion pj)
  (if (personaje? pj)
      (car pj)
      null
      )
  )
;
;
;
(define (getOrientacion pj)
  (if (personaje? pj)
      (cadr pj)
      null
      )
  )
;
;
;
(define (getVida pj)
  (if (personaje? pj)
      (caddr pj)
      null
      )
  )
;
;
;
(define (getName pj)
  (if (personaje? pj)
      (cadddr pj)
      null
      )
  )
;
;
;
(define (setPosicionX pj x)
  (if (personaje? pj)
      (personaje (cons x (cdr (getPosicion pj))) (getOrientacion pj) (getVida pj) (getName pj))
      null
      )
  )
;
;
;
(define (setPosicionY pj y)
  (if (personaje? pj)
      (personaje (cons (car (getPosicion pj)) y) (getOrientacion pj) (getVida pj) (getName pj))
      null
      )
  )
;
;
;
(define (setOrientacion pj O)
  (if (personaje? pj)
      (personaje (cons (car (getPosicion pj)) (cdr (getPosicion pj))) O (getVida pj) (getName pj))
      null
      )
  )
;
;
;
(define (setVida pj vida)
  (if (personaje? pj)
      (personaje (cons (car (getPosicion pj)) (cdr (getPosicion pj))) (getOrientacion pj) vida (getName pj))
      null
      )
  )

(define a 1103515245)
(define c 12345)
(define m 2147483648)
;
;
;
(define myRandom
  (lambda
    (xn)
    (remainder (+ (* a xn) c) m)
  )
)
;
;
;
(define (generateEnemigosRL E N M seed)
  (let ([Y (remainder (myRandom (* seed 5)) M)]
        [X (remainder (myRandom (* seed 4)) N)]
       )
    (if (= E 0)
        null
        (cons (personaje (cons X Y) 1 100 "E") (generateEnemigosRL (- E 1) E N M))        
        )
    )
)
;
;
;
(define (generateTeamRL E N M seed)
  (let ([B (remainder (myRandom (* seed 6)) M)]
        [A (remainder (myRandom (* seed 7)) N)]
       )
    (if (= E 0)
        null
        (cons (personaje (cons A B) 0 100 "A") (generateTeamRL (- E 1) E N M))        
        )
    )
)
;
;
;
(define (createScene N M E D seed)
     (list "PLAYING" N M (list (generateEnemigosRL E N M seed)) (list (generateTeamRL E N M seed)))
  )