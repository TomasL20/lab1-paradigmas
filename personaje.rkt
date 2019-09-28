#lang racket
;TDA PERSONAJE
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
      (personaje (cons (+ (car (getPosicion pj)) x) (cdr (getPosicion pj))) (getOrientacion pj) (getVida pj) (getName pj))
      null
      )
  )
;
;
;
(define (setPosicionY pj y)
  (if (personaje? pj)
      (personaje (cons (car (getPosicion pj)) (+ (cdr (getPosicion pj)) y)) (getOrientacion pj) (getVida pj) (getName pj))
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
;TDA PROYECTIL 
;entrada : pair x number x number x number
;salida : TDA proyectil
;objetivo : construir el TDA proyectil
(define (proyectil position angle speed damage)
  (if (and (pair? position) (number? angle) (number? damage) (number? speed))
      (list (cons (+ (car position) (* (cos (* angle (/ pi 180))) speed)) (+ (cdr position) (* (sin (* angle (/ pi 180))) speed) (* (/ 1 2) (* -1 9.81)))) angle speed damage)
      null
      )
  )
;entrada : proyectil
;salida : bool
;objetivo : verificar si la entrada corresponde al TDA proyectil
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
;entrada : proyectil
;salida : pair
;objetivo : obtener la posición del proyectil
(define (getPosition p)
  (if (proyectil? p)
      (car p)
      null
      )
  )
;entrada : proyectil
;salida : number 
;objetivo : obtener el ángulo de lanzamiento del proyectil
(define (getAngle p)
  (if (proyectil? p)
      (cadr p)
      null
      )
  )
;entrada : proyectil
;salida : number 
;objetivo : obtener la velocidad de lanzamiento del proyectil
(define (getSpeed p)
  (if (proyectil? p)
      (caddr p)
      null
      )
  )
;entrada : proyectil 
;salida : number
;objetivo : obtener el daño que causa el proyectil 
(define (getDamage p)
  (if (proyectil? p)
      (cadddr p)
      null
      )
  )
;entrada : proyectil x integer number
;salida : proyectil 
;objetivo : modificar la posición del proyectil en el eje x
(define (setX p x)
  (if (proyectil? p)
      (proyectil (cons x (cdr (getPosition p))) (getAngle p) (getSpeed p) (getDamage p))
      null
      )
  )
;entrada : proyectil x number 
;salida : proyectil 
;objetivo : 
(define (setY p y)
  (if (proyectil? p)
      (proyectil (cons (car (getPosition p)) y) (getAngle p) (getSpeed p) (getDamage p))
      null
      )
  )
;entrada : proyectil x number 
;salida : proyectil 
;objetivo : modificar el ángulo de lanzamiento del proyectil
(define (setAngle p a)
  (if (proyectil? p)
      (proyectil (cons (car (getPosition p)) (cdr (getPosition p))) a (getSpeed p) (getDamage p))
      null
      )
  )
;entrada : proyectil x number
;salida : proyectil 
;objetivo : modificar la velocidad de lanzamiento del proyectil
(define (setSpeed p s)
  (if (proyectil? p)
      (proyectil (cons (car (getPosition p)) (cdr (getPosition p))) (getAngle p) s (getDamage p))
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
  (let ([Y (remainder (myRandom (* seed 75)) M)]
        [aleatory (remainder (myRandom (* seed 3)) 359)]
        )
    (if (= E 0)
        null
        (cons (personaje (cons (quotient N 2) Y) 1 100 "E") (generateEnemigosRL (- E 1) N M aleatory))        
        )
    )
  )
;
;
;
(define (generateTeamRL E N M seed)
  (let ([B (remainder (myRandom (* seed 7)) M)]
        [aleatory (remainder (myRandom (* seed 3)) 359)]
        )
    (if (= E 0)
        null
        (cons (personaje (cons (quotient N 2) B) 0 100 "A") (generateTeamRL (- E 1) N M aleatory))        
        )
    )
  )
;
;
;
(define (createScene N M E D seed)
  (list "PLAYING" N M (generateTeamRL E N M seed) (generateEnemigosRL E N M seed))
  )
;
;
;
(define (verificarEquipo equipo1 equipo2)
  (if (null? equipo1)
      #t
      (if (verificarPosicion (car (car equipo1)) equipo2)
          (verificarEquipo (cdr equipo1) equipo2)
          #f
          )
      )
  )
;
;
;
(define (verificarPosicion pos equipo2)
  (if (null? equipo2)
      #t
      (if (equal? pos (car (car equipo2)))
          #f
          (verificarPosicion pos (cdr equipo2))
          )
      )
  )
;
;
;
(define (checkScene scene)
  (if (and (list? scene) (= (length scene) 5))
      (if (and (string? (car scene)) (positive-integer? (cadr scene)) (positive-integer? (caddr scene)) (list? (cadddr scene)) (list? (last scene)))
          (if (verificarEquipo (cadddr scene) (last scene))
              #t
              #f
              )
          #f
          )
      #f
      )
  )
;
;
;
(define (getState scene)
  (car scene)
  )
;
;
;
(define (getN scene)
  (cadr scene)
  )
;
;
;
(define (getM scene)
  (caddr scene)
  )
;
;
;
(define (getTeam scene aux) ; aux = 0 para team y 1 para enemy
  (if (= aux 0)
      (cadddr scene)
      (last scene)
      )
  )
;
;
;
(define (getMember team member)
  (define (selectPj team member aux)
    (if (= member aux)
        (car team)
        (selectPj (cdr team) member (+ aux 1))
        )
    )
  (selectPj team member 1)
  )
;
;
;
(define (play scene)
  (lambda (member) (lambda (move) (lambda (tf) (lambda (angle) (lambda (seed)
                                                                 (shoot (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) tf (proyectil (cons 7 3) 50 30 20))
                                                                 ))))))
;entrada = pair x lista
;salida = bool
;objetivo = comprobar si una posición es igual a alguna posición de un personaje del equipo recibido
(define (comparar pos equipo)
  (if (null? equipo)
      #f
      (if (equal? pos (car (car equipo)))
          #t
          (comparar pos (cdr equipo))
          )
      )
  )
;entrada= lista 
;salida= string
;objetivo = representar el escenario como un string donde se puedan identificar los elementos del escenario 
(define (scene>string scene)
  (define (generarString scene fila columna string)
    (cond
      [(> fila (getN scene)) string]
      [(> columna (getM scene)) (generarString scene (+ fila 1) 0 (string-append string "\n"))]
      [(comparar (cons fila columna) (getTeam scene 0)) (generarString scene fila (+ columna 1) (string-append string "A"))]
      [(comparar (cons fila columna) (getTeam scene 1)) (generarString scene fila (+ columna 1) (string-append string "E"))]
      [(> fila (quotient (getN scene) 2)) (generarString scene fila (+ columna 1) (string-append string "#"))]
      [else (generarString scene fila (+ columna 1) (string-append string "_"))]
      )
    )
  (generarString scene 0 0 "")
  )
;entrada = personaje x lista x positive integer x positive integer x lista 
;salida = lista
;objetivo = actualizar la lista de aliados o enemigos después de haber realizado algún movimiento o de haber recibido un proyectil
(define (setEquipo pj equipo member aux lista)
  (cond
    [(null? equipo) lista]
    [(= aux member) (setEquipo pj (cdr equipo) member (+ aux 1) (append lista (list pj)))]
    [else (setEquipo pj (cdr equipo) member (+ aux 1) (append lista (list (car equipo))))]
    )
  )
;entrada = lista x lista x positive integer x positive integer x lista
;salida = lista 
;objetivo = actualizar el escenario con la nueva lista actualizada de aliados o personajes 
(define (setScene scene equipo aux contador newScene)
  (cond
    [(null? scene) newScene]
    [(= aux contador) (setScene (cdr scene) equipo aux (+ contador 1) (append newScene (list equipo)))]
    [else (setScene (cdr scene) equipo aux (+ contador 1) (append newScene (list (car scene))))]
    )
  )

(define S1 (createScene 30 30 3 2 748357483))
(define E1 (getTeam S1 0))
(define P1 (setPosicionY (getMember E1 2) 3))
(define E2 (setEquipo P1 E1 2 1 '()))
(define S2 (setScene S1 E2 4 1 '()))
;
;
;
(define (shoot scene posP projectile)
  (cond
    [(comparar posP (getTeam scene 0)) (setScene scene (setEquipo (setVida (car (findTarget posP (getTeam scene 0) 1)) (- (getVida (car (findTarget posP (getTeam scene 0) 1))) (getDamage projectile))) (getTeam scene 0) (cdr (findTarget posP (getTeam scene 0) 1)) 1 '()) 4 1 '())]
    [(comparar posP (getTeam scene 1)) (setScene scene (setEquipo (setVida (car (findTarget posP (getTeam scene 1) 1)) (- (getVida (car (findTarget posP (getTeam scene 1) 1))) (getDamage projectile))) (getTeam scene 1) (cdr (findTarget posP (getTeam scene 1) 1)) 1 '()) 5 1 '())]
    [else scene]
    )
  )
;
;
;
(define (findTarget posP equipo contador)
  (if (equal? posP (getPosicion (car equipo)))
      (cons (car equipo) contador)
      (findTarget posP (cdr equipo) (+ contador 1))
      )
  )
