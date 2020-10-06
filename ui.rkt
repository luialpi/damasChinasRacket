#lang racket

(require 2htdp/image)
(require graphics/graphics)
(open-graphics)

(define alto 1000)
(define cir-pos-inicial ( - ( / alto 2 ) 50 ))
(define ventana1 (open-viewport "AJEDREZ" alto alto))

;dibuja los circulos grises del tablero inicial
(define dibujar-circulos
  (for ([i tablero-ui] )
    ( for ([j i])
     ((draw-solid-ellipse ventana1) (make-posn (list-ref j 0) (list-ref j 1) ) 50 50 "DimGray"))
))

;retorna la posicion (i, j) en el ui para dibujar el circulo deseado
(define (get-ui-pos i j)
  (list (+ 10 (+ (* 50 j) (* 50 i))) (+ 450 (- (* 50 j) (* 50 i)))))

;dibuja 
(define (mueve-ficha color i j) (
      (draw-solid-ellipse ventana1) (make-posn (list-ref (get-ui-pos i j) 0) (list-ref (get-ui-pos i j) 1) ) 50 50 color) )

(mueve-ficha "blue" 9 9)
(mueve-ficha "blue" 0 0)
