#lang racket

; —————————————————————————————————
; seccion de imports y variables
; —————————————————————————————————

(require graphics/graphics)
(require htdp/matrix)
(open-graphics)

;;tamanios de la ventana
(define alto 1000)
(define ancho 1200)

;;Ventana para mostrar el tablero
(define ventana1 (open-viewport "Damas Chinas" alto ancho))

;;se utiliza para saber el tamanio de la matriz, expecificamente para el ui
(define len '(0 1 2 3 4 5 6 7 8 9))

;tablero de juego
(define tablero (build-matrix 10 10 (lambda (x y) (* x 0))))


; —————————————————————————————————
; seccion de logica de juego
; —————————————————————————————————
;revisa si hay una ficha en el lugar i j
(define (check-posicion i j) (= (matrix-ref tablero i j) 0))

;;Pone una ficha en la posicion deseada del tablero
;;Verifica si ya hay una ficha en el lugar
(define (set-posicion i j ficha)
  (cond
    [(check-posicion i j) (set! tablero (matrix-set tablero i j ficha))]
    [else (print "No se puede mover, ya hay una ficha ahi.")]
 ))

;;verifica si la diferencia en distancia entre dos movimientos es valida
(define (check-distancia origen destino)
  (define x (abs (- (list-ref origen 0) (list-ref destino 0))))
  (define y (abs (- (list-ref origen 1) (list-ref destino 1))))
  (or (= x 2) (= x 1) (= y 2) (= y 1))

)

;;verifica si un movimiento es valido
;retorna true en caso de que el movimiento sea valido
(define (check-movimiento origen destino)
  (cond
    [(not (and (list? origen) (list? destino))) false ]
    [ (or (check-posicion (list-ref origen 0) (list-ref origen 1)) (not (check-posicion (list-ref destino 0) (list-ref destino 1)))) false]
    [else
     ;;verificamos los saltos
     (cond
       ;;verifico la posicion destino(1) +1
       [(and (> (list-ref origen 1) (list-ref destino 1)) (check-distancia origen  destino))
        (not (check-posicion (list-ref destino 0) (+ 1 (list-ref destino 1)))) ]

       ;;verifico la posicion destino(0) +1
       [(and (> (list-ref origen 0) (list-ref destino 0)) (check-distancia origen  destino))
        (not (check-posicion (+ 1 (list-ref destino 0)) (list-ref destino 1)))]

       ;;verifico la posicion origen(1) +1
       [(and (< (list-ref origen 1) (list-ref destino 1)) (check-distancia destino origen))
        (not (check-posicion (list-ref origen 0) (+ 1 (list-ref origen 1))))]

       ;;verifico la posicion origen(1) +1
       [(and (< (list-ref origen 0) (list-ref destino 0)) (check-distancia destino  origen))
        (not (check-posicion (+ 1 (list-ref destino 0)) (list-ref destino 1)))]

       ;else, si no hay saltos, verificamos que el destino sea posible mover
       [else (and (check-posicion (list-ref destino 0) (list-ref destino 1)) (check-distancia origen destino)) ]
       )
     ]
    )
  )

;;recorre la lista de movimientos verificando si es valido '( '(i j) '(i j) '(i j))
;;esta se utiliza para cada ronda de movidas
(define (check-movimientos lista)
  (cond
    [(or (empty? lista) (<= (length lista ) 1)) true]
    [else
     ( cond
        [ (check-movimiento (first lista) (second lista)) (check-movimientos (cdr lista))]
        [else false]
      )]
    )
  )



; —————————————————————————————————
; seccion de interfaz grafica
; —————————————————————————————————

;retorna la posicion (i, j) en el ui para dibujar el circulo deseado
(define (get-ui-pos i j)
  (list (+ 480 (- (* 50 j) (* 50 i))) (+ 25 (+ (* 50 j) (* 50 i)))))

;dibuja ficha de color y posicion deseada
(define (mueve-ficha color i j) (
      (draw-solid-ellipse ventana1) (make-posn (list-ref (get-ui-pos i j) 0) (list-ref (get-ui-pos i j) 1) ) 50 50 color) )

;; define los valores iniciales

(define iniciar-matriz
  (for ([i len] )
    ( for ([j len])
       (cond
         [(or (and (= i 0) (<= j 3)) (and (= i 1) (<= j 2)) (and (= i 2) (<= j 1)) (and (= i 3) (= j 0)))
          ;pone la ficha en el luhar del tablero
          (set-posicion i j 1)]
         [ (or (and (= i 9) (>= j 6)) (and (= i 8) (>= j 7)) (and (= i 7) (>= j 8)) (and (= i 6) (= j 9)))
          ;pone la ficha en el luhar del tablero
          (set-posicion i j 2)]
     ))
))

;dibuja los circulos grises del tablero inicial
(define dibujar-circulos
  (for ([i len] )
    ( for ([j len])
       (cond
         [(= (matrix-ref tablero i j) 1)
          (mueve-ficha "blue" i j)]
         [ (= (matrix-ref tablero i j) 2)
          (mueve-ficha "red" i j)]
         [else
          (mueve-ficha "DimGray" i j)
          ]
     ))
))

;;dibuja los numeros para mejor ubicacion a nivel de ui
(define dibujar-numeros
  (for ([i len])
    ((draw-string ventana1) (make-posn (list-ref (get-ui-pos i 0) 0) (list-ref (get-ui-pos i 0) 1)) (number->string i)) 
    ((draw-string ventana1) (make-posn (+ 45 (list-ref (get-ui-pos 0 i) 0)) (- (list-ref (get-ui-pos 0 i) 1) 2)) (number->string i))
    )
)
