#lang racket
;entrada = entero x entero x entero x entero x string
;salida = personaje
;objetivo = construir el TDA personaje 
(define (personaje x y orientacion vida identidad)
  (if (and (string? identidad) (integer? x) (integer? y) (integer? orientacion) (number? vida))
      (list (cons x y) orientacion vida identidad)
      null
      )
  )
;entrada = personaje
;salida = bool
;objetivo = verificar la entrada pertenece a un personaje
(define (personaje? pj)
  (if (and (list? pj) (= (length pj) 4))
      (if (and (pair? (car pj)) (integer? (cadr pj)) (number? (caddr pj)) (string? (cadddr pj))
               (integer? (car (car pj))) (integer? (cdr (car pj)))
               (and (<= (caddr pj) 100) (> (caddr pj) 0)))
          #t
          #f
          )
      #f
      )
  )
;entrada = personaje
;salida = pair
;salida = obtener la posición del personaje
(define (getPosicion pj)
  (if (personaje? pj)
      (car pj)
      null
      )
  )
;entrada = personaje
;salida = entero
;objetivo = obtener la orientación del personaje (0 hacia la derecha y 1 hacia la izquierda) (vista)
(define (getOrientacion pj)
  (if (personaje? pj)
      (cadr pj)
      null
      )
  )
;entrada = personaje
;salida = number
;objetivo = obtener la vida del personaje
(define (getVida pj)
  (if (personaje? pj)
      (caddr pj)
      null
      )
  )
;entrada = personaje
;salida = string
;objetivo = obtener el nombre del personaje
(define (getName pj)
  (if (personaje? pj)
      (cadddr pj)
      null
      )
  )
;entrada = personaje x entero
;salida = personaje
;objetivo = modificar la posición del personaje en el eje vertical
(define (setPosicionX pj x)
  (if (personaje? pj)
      (personaje (cons (+ (car (getPosicion pj)) x) (cdr (getPosicion pj))) (getOrientacion pj) (getVida pj) (getName pj))
      null
      )
  )
;entrada = personaje x entero
;salida = personaje
;objetivo = modificar la posición del personaje en el eje horizontal
(define (setPosicionY pj y)
  (if (personaje? pj)
      (personaje (car (getPosicion pj)) (+ (cdr (getPosicion pj)) y) (getOrientacion pj) (getVida pj) (getName pj))
      null
      )
  )
;entrada = personaje x entero
;salida = personaje
;objetivo = modificar la orientación del personaje
(define (setOrientacion pj O)
  (if (personaje? pj)
      (personaje (cons (car (getPosicion pj)) (cdr (getPosicion pj))) O (getVida pj) (getName pj))
      null
      )
  )
;entrada = personaje x number 
;salida = personaje
;objetivo = modificar la vida de un personaje
(define (setVida pj vida)
  (if (personaje? pj)
      (personaje (car (getPosicion pj)) (cdr (getPosicion pj)) (getOrientacion pj) vida (getName pj))
      null
      )
  )
;TDA PROYECTIL 
;entrada : pair x number x number x number
;salida : TDA proyectil
;objetivo : construir el TDA proyectil
(define (proyectil x y angle speed damage)
  (if (and (number? x) (number? y) (number? angle) (number? damage) (number? speed))
      (list (cons x y) angle speed damage)
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
;objetivo : modificar la posición del proyectil en el eje y
(define (setX p x)
  (if (proyectil? p)
      (proyectil (cons x (cdr (getPosition p))) (getAngle p) (getSpeed p) (getDamage p))
      null
      )
  )
;entrada : proyectil x number 
;salida : proyectil 
;objetivo : modificar la posición del proyectil en el eje x
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
(define myRandom
  (lambda
      (xn)
    (remainder (+ (* a xn) c) m)
    )
  )
;entrada = entero x entero x entero x entero
;salida = lista de personajes (enemigos)
;objetivo = generar a los enemigos
;recursión = recursión lineal
(define (generateEnemies E N M seed)
  (let ([Y (remainder (myRandom (* seed 4)) M)]
        [aleatory (remainder (myRandom (* seed 3)) 359)]
        )
    (if (= E 0)
        null
        (cons (personaje (quotient N 2) Y 1 100 "E") (generateEnemies (- E 1) N M aleatory))        
        )
    )
  )
;entrada = entero x entero x entero x entero
;salida = lista de personajes (aliados)
;objetivo = generar a los personajes controlados por el jugador
;recursión = recursión lineal
(define (generateAllies E N M seed)
  (let ([Y (remainder (myRandom (* seed 7)) M)]
        [aleatory (remainder (myRandom (* seed 3)) 359)]
        )
    (if (= E 0)
        null
        (cons (personaje (quotient N 2) Y 0 100 "A") (generateAllies (- E 1) N M aleatory))        
        )
    )
  )
;entrada = entero x entero x entero x entero
;salida = lista de proyectiles
;objetivo = generar los proyectiles para cada aliado
;recursión = recursión lineal
(define (generateProjectilesA E N M seed)
  (let ([Y (remainder (myRandom (* seed 7)) M)]
        [aleatory (remainder (myRandom (* seed 3)) 359)]
        )
    (if (= E 0)
        null
        (cons (proyectil (quotient N 2) Y 0 30 20) (generateProjectilesA (- E 1) N M aleatory))
        )
    )
  )
;entrada = entero x entero x entero x entero
;salida = lista de proyectiles
;objetivo = generar los proyectiles para cada enemigo
;recursión = recursión lineal 
(define (generateProjectilesE E N M seed)
  (let ([Y (remainder (myRandom (* seed 4)) M)]
        [aleatory (remainder (myRandom (* seed 3)) 359)]
        )
    (if (= E 0)
        null
        (cons (proyectil (quotient N 2) Y 0 30 20) (generateProjectilesE (- E 1) N M aleatory))        
        )
    )
  )
;entrada = lista de personajes x entero
;salida = personaje
;objetivo = obtener el personaje seleccionado del equipo
;recursión = recursión de cola
(define (getMember team member)
  (define (selectPj team member aux)
    (if (= member aux)
        (car team)
        (selectPj (cdr team) member (+ aux 1))
        )
    )
  (selectPj team member 1)
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
;entrada = proyectil x lista de proyectiles x entero x entero
;salida = lista de proyectiles
;objetivo = actualizar la lista de proyectiles después de un disparo
;recursión = recursión de cola
(define (setProjectiles p projectiles member aux contador)
  (cond
    [(= aux 0) (setEquipo p (car projectiles) member contador '())]
    [(= aux 1) (setEquipo p (cdr projectiles) member contador '())]
    )
  )
;entrada = equipo x entero x lista
;salida = equipo
;objetivo = sacar a personajes del equipo si tienen vida menor o igual a 0 o si se sale del escenario
;recursión = recursión de cola
(define (removeMember N M equipo aux newEquipo)
  (cond
    [(null? equipo) newEquipo]
    [(or (<= (getVida (getMember equipo aux)) 0) (> (car (getPosicion (getMember equipo aux))) N) (> (cdr (getPosicion (getMember equipo aux))) M)) (removeMember (cdr equipo) (+ aux 1) newEquipo)]
    [else (removeMember (cdr equipo) (+ aux 1) (append newEquipo (list (car equipo))))]
    )
  )
;entrada = par x lista de personajes x entero
;salida = par 
;objetivo = obtener el personaje dañado por el proyectil y su posición en la lista del equipo de personajes 
;recursion = recursión de cola
(define (findTarget posP equipo contador)
  (if (equal? posP (getPosicion (car equipo)))
      (cons (car equipo) contador)
      (findTarget posP (cdr equipo) (+ contador 1))
      )
  )
;TDA escenario
;entrada = entero x entero x entero x entero x entero
;salida = escenario
;objetivo = construir el escenario del juego
;recursión = recursión lineal
(define (createScene N M E D seed)
  (list "PLAYING" N M (generateAllies E N M seed) (generateEnemies E N M seed) (cons (generateProjectilesA E N M seed) (generateProjectilesE E N M seed)))
  )
;entrada = escenario
;salida = bool
;objetivo = verificar si la entrada pertenece a un escenario jugable
;recursión = recursión de cola
(define (checkScene scene)
  (if (and (list? scene) (= (length scene) 6))
      (if (and (string? (car scene)) (positive-integer? (cadr scene)) (positive-integer? (caddr scene)) (list? (cadddr scene)) (list? (cadddr (cdr scene))) (pair? (last scene)))
          (if (verificarEquipo (cadddr scene) (cadddr (cdr scene)))
              #t
              #f
              )
          #f
          )
      #f
      )
  )
;entrada = escenario
;salida = string
;objetivo = obtener el estado del juego 
(define (getState scene)
  (car scene)
  )
;entrada = escenario
;salida = entero
;objetivo = obtener las filas del escenario
(define (getN scene)
  (cadr scene)
  )
;entrada = escenario
;salida = entero
;objetivo = obtener las columnas del escenario
(define (getM scene)
  (caddr scene)
  )
;entrada = escenario x entero
;salida = lista de personajes
;objetivo = obtener a los aliados o enemigos 
(define (getTeam scene aux) ; aux = 0 para allies y 1 para enemies
  (if (= aux 0)
      (cadddr scene)
      (cadddr (cdr scene))
      )
  )
;entrada = escenario
;salida = lista de proyectiles
;objetivo = obtener los proyectiles en juego
(define (getProjectiles scene)
  (last scene)
  )
;entrada = lista de proyectiles x entero
;salida = proyectil
;objetivo = obtener el proyectil de un personaje
;recursión = recursión de cola
(define (selectProjectil proyectiles n aux)
  (if (= n aux)
      (car proyectiles)
      (selectProjectil (cdr proyectiles) n (+ aux 1))
      )
  )
;entrada = escena
;salida = escena
;objetivo = modificar el estado del juego
(define (setState scene)
  (cond
    [(and (= (length (getTeam scene 0)) 0) (= (length (getTeam scene 1)) 0)) (list "DRAW" (getN scene) (getM scene) (getTeam scene 0) (getTeam scene 1) (getProjectiles scene))]
    [(= (length (getTeam scene 0)) 0) (list "DEFEAT" (getN scene) (getM scene) (getTeam scene 0) (getTeam scene 1) (getProjectiles scene))]
    [(= (length (getTeam scene 1)) 0) (list "VICTORY" (getN scene) (getM scene) (getTeam scene 0) (getTeam scene 1) (getProjectiles scene))]
    [else scene]
    )
  )
;entrada = lista x lista x positive integer x positive integer x lista
;salida = lista 
;objetivo = actualizar el escenario con la nueva lista actualizada de aliados o personajes
;recursion =
(define (setScene scene equipo aux contador newScene)
  (cond
    [(null? scene) newScene]
    [(= aux contador) (setScene (cdr scene) equipo aux (+ contador 1) (append newScene (list equipo)))]
    [else (setScene (cdr scene) equipo aux (+ contador 1) (append newScene (list (car scene))))]
    )
  )
;entrada = scene x pair x proyectil
;salida = scene
;objetivo = disparar el proyectil y actualizar el escenario 
(define (shoot scene tf)
  (cond
    [(comparar tf (getTeam scene 0)) (setScene scene (setEquipo (setVida (car (findTarget tf (getTeam scene 0) 1)) (- (getVida (car (findTarget tf (getTeam scene 0) 1))) (getDamage (last (car (getProjectiles scene)))))) (getTeam scene 0) (cdr (findTarget tf (getTeam scene 0) 1)) 1 '()) 4 1 '())]
    [(comparar tf (getTeam scene 1)) (setScene scene (setEquipo (setVida (car (findTarget tf (getTeam scene 1) 1)) (- (getVida (car (findTarget tf (getTeam scene 1) 1))) (getDamage (last (car (getProjectiles scene)))))) (getTeam scene 1) (cdr (findTarget tf (getTeam scene 1) 1)) 1 '()) 5 1 '())]
    [else scene]
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
;entrada = escenario x entero x entero x tf x entero x entero
;salida = escenario
;objetivo = ejecutar el turno del jugador y del enemigo 
(define (play scene)
  (lambda (member) (lambda (move) (lambda (tf) (lambda (angle) (lambda (seed)
                                                                 (turnoE (setScene (shoot (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) member angle 0 0 seed)) (cons (setProjectiles (setXY (selectProjectil (car (getProjectiles (shoot (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) member angle 0 0 seed)))) member 1) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) member angle 0 0 seed)) (getProjectiles (shoot (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) member angle 0 0 seed))) member 0 1) (cdr (getProjectiles (shoot (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 0) member) move) (getTeam scene 0) member 1 '()) 4 1 '()) member angle 0 0 seed))))) 6 1 '())  member move tf angle seed)
                                                                 ))))))
;entrada = escenario x entero x entero x tf x entero x entero
;salida = escenario
;objetivo = ejecutar el turno del enemigo 
(define (turnoE scene member move tf angle seed)
  (setScene (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 1) member) move) (getTeam scene 1) member 1 '()) 5 1 '()) (cons (car (getProjectiles scene)) (setProjectiles (setXY (selectProjectil (cdr (getProjectiles (shoot (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 1) member) move) (getTeam scene 1) member 1 '()) 5 1 '()) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 1) member) move) (getTeam scene 1) member 1 '()) 5 1 '()) member angle 0 1 seed))
  )) member 1) (tf (setScene scene (setEquipo (setPosicionY (getMember (getTeam scene 1) member) move) (getTeam scene 1) member 1 '()) 5 1 '()) member angle 0 1 seed)) (getProjectiles scene) member 1 1)) 6 1 '())
  )
;entrada = escenario x entero x entero x tf x entero x entero
;salida =  lista infinita de escenarios
;objetivos = poder ver el escenario cada cierto tiempo t  
(define (playLazy scene member move tf t angle seed)
  null
  )
(define S1 (createScene 30 30 3 2 748357483))
;entrada = entero
;salida = entero
;objetivo = generar un ángulo en radianes entre 0 y 90°
(define (angleAleatory seed)
  (* (remainder (myRandom (* seed 3)) 90) (/ pi 180))
  )
;entrada = escenario x entero x entero x entero x entero x entero
;salida = pair
;objetivo = generar el punto de impacto del proyectil o generar el punto del proyectil en un tiempo t
(define (tf scene member angle aux1 aux2 seed)
   (cond
     [(and (= aux1 0) (= aux2 0)) (cons (car (getPosition (selectProjectil (car (getProjectiles scene)) member 1))) (+ (floor (/ (* (sin (* (* angle (/ pi 180)) 2)) (expt 30 2)) 9.81)) (cdr (getPosition (selectProjectil (cdr (getProjectiles scene)) member 1)))))] ;modo play y no aleatorio
     [(and (= aux1 0) (= aux2 1)) (cons (car (getPosition (selectProjectil (cdr (getProjectiles scene)) member 1))) (+ (floor (/ (* (sin (* (angleAleatory seed) 2)) (expt 30 2)) 9.81)) (cdr (getPosition (selectProjectil (cdr (getProjectiles scene)) member 1)))))] ;modo play y aleatorio
     [(and (= aux1 1) (= aux2 0))] ; modo playLazy y no aleatorio
     ; modo playLazy y aleatorio 
     )
  )
;entrada = lista de personajes x lista de personajes
;salida = bool
;objetivo = verificar que los aliados y enemigos tengan posiciones distintas
;recursión = recursión de cola
(define (verificarEquipo equipo1 equipo2)
  (if (null? equipo1)
      #t
      (if (verificarPosicion (car (car equipo1)) equipo2)
          (verificarEquipo (cdr equipo1) equipo2)
          #f
          )
      )
  )
;entrada = pair x lista de personajes
;salida = bool
;objetivo = verificar que un personaje del equipo aliados tenga posición distinta a la de los enemigos
;recursión = recursión de cola
(define (verificarPosicion pos equipo2)
  (if (null? equipo2)
      #t
      (if (equal? pos (car (car equipo2)))
          #f
          (verificarPosicion pos (cdr equipo2))
          )
      )
  )
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
;
;
;
(define (setXY p coord)
  (if (proyectil? p)
      (proyectil (car coord) (cdr coord) (getAngle p) (getSpeed p) (getDamage p))
      null
      )
  )
