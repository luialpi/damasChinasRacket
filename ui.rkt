#lang racket

; —————————————————————————————————
; seccion de imports y variables
(require graphics/graphics)
(open-graphics)

(define alto 1000)
(define ancho 1200)
(define cir-pos-inicial ( - ( / alto 2 ) 50 ))
(define ventana1 (open-viewport "Damas Chinas" alto ancho))
(define len '(0 1 2 3 4 5 6 7 8 9))


; —————————————————————————————————
; seccion de interfaz grafica

;retorna la posicion (i, j) en el ui para dibujar el circulo deseado
(define (get-ui-pos i j)
  (list (+ 25 (+ (* 50 j) (* 50 i))) (+ 480 (- (* 50 j) (* 50 i)))))

;dibuja ficha de color y posicion deseada
(define (mueve-ficha color i j) (
      (draw-solid-ellipse ventana1) (make-posn (list-ref (get-ui-pos i j) 0) (list-ref (get-ui-pos i j) 1) ) 50 50 color) )

;dibuja los circulos grises del tablero inicial
(define dibujar-circulos
  (for ([i len] )
    ( for ([j len])
       (cond
         [(or (and (= i 0) (<= j 3)) (and (= i 1) (<= j 2)) (and (= i 2) (<= j 1)) (and (= i 3) (= j 0)))
          (mueve-ficha "blue" i j) ]
         [ (or (and (= i 9) (>= j 6)) (and (= i 8) (>= j 7)) (and (= i 7) (>= j 8)) (and (= i 6) (= j 9)))
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
    ((draw-string ventana1) (make-posn (list-ref (get-ui-pos 0 i) 0) (+ 60 (list-ref (get-ui-pos 0 i) 1))) (number->string i))
    )
)


; —————————————————————————————————
; seccion de logica de juego
