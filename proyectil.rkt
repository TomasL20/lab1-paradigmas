#lang racket
;
;
;
(define (proyectil position angle speed damage)
  (if (and (pair? position) (number? angle) (number? damage) (number? speed))
      (list (cons (+ (car position) (* (cos (* angle (/ pi 180))) speed)) (+ (cdr position) (* (sin (* angle (/ pi 180))) speed) (* (/ 1 2) (* -1 9.81)))) angle speed damage)
      null
      )
  )
;
;
;
(define (proyectil? p)
  (if (and (list? p) (= (length p) 4))
      (if (and (and (pair? (car p)) (number? (car (car p))) (number? (cdr (car p))))
               (number? (cadr p))
               (number? (caddr p))
               (number? (cadddr p)))
          #t
          #f
          )
      #f
      )
  )
;
;
;
(define (getPosition p)
  (if (proyectil? p)
      (car p)
      null
      )
  )
;
;
;
(define (getAngle p)
  (if (proyectil? p)
      (cadr p)
      null
      )
  )
;
;
;
(define (getSpeed p)
  (if (proyectil? p)
      (caddr p)
      null
      )
  )
;
;
;
(define (getDamage p)
  (if (proyectil? p)
      (cadddr p)
      null
      )
  )
;
;
;
(define (setX p x)
  (if (proyectil? p)
      (proyectil (cons x (cdr (getPosition p))) (getAngle p) (getSpeed p) (getDamage p))
      null
      )
  )
;
;
;
(define (setY p y)
  (if (proyectil? p)
      (proyectil (cons (car (getPosition p)) y) (getAngle p) (getSpeed p) (getDamage p))
      null
      )
  )
;
;
;
(define (setAngle p a)
  (if (proyectil? p)
      (proyectil (cons (car (getPosition p)) (cdr (getPosition p))) a (getSpeed p) (getDamage p))
      null
      )
  )
;
;
;
(define (setSpeed p s)
  (if (proyectil? p)
      (proyectil (cons (car (getPosition p)) (cdr (getPosition p))) (getAngle p) s (getDamage p))
      null
      )
  )
