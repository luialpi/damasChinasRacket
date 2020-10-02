#lang racket

(require 2htdp/image)
(require graphics/graphics)
(open-graphics)

(define alto 1000)
(define cir-pos-inicial ( - ( / alto 2 ) 50 ))
(define ventana1 (open-viewport "AJEDREZ" alto alto))

(define tablero
  (list
   (list '(10 450) '(60 400) '(110 350) '(160 300) '(210 250) '(260 200) '(310 150) '(360 100) '(410 50) '(460 0)  )
   (list '(60 500) '(110 450) '(160 400) '(210 350) '(260 300) '(310 250) '(360 200) '(410 150) '(460 100) '(510 50) )
   (list '(110 550) '(160 500) '(210 450) '(260 400) '(310 350) '(360 300) '(410 250) '(460 200) '(510 150) '(560 100) )
   (list '(160 600) '(210 550) '(260 500) '(310 450) '(360 400) '(410 350) '(460 300) '(510 250) '(560 200) '(610 150) )
   (list '(210 650) '(260 600) '(310 550) '(360 500) '(410 450) '(460 400) '(510 350) '(560 300) '(610 250) '(660 200) )
   (list '(260 700) '(310 650) '(360 600) '(410 550) '(460 500) '(510 450) '(560 400) '(610 350) '(660 300) '(710 250) )
   (list '(310 750) '(360 700) '(410 650) '(460 600) '(510 550) '(560 500) '(610 450) '(660 400) '(710 350) '(760 300) )
   (list '(360 800) '(410 750) '(460 700) '(510 650) '(560 600) '(610 550) '(660 500) '(710 450) '(760 400) '(810 350) )
   (list '(410 850) '(460 800) '(510 750) '(560 700) '(610 650) '(660 600) '(710 550) '(760 500) '(810 450) '(860 400) )
   (list '(460 900) '(510 850) '(560 800) '(610 750) '(660 700) '(710 650) '(760 600) '(810 550) '(860 500) '(910 450) )
   )
)

(define dibujar-circulos
  (for ([i tablero] )
    ( for ([j i])
     ((draw-solid-ellipse ventana1) (make-posn (list-ref j 0) (list-ref j 1) ) 50 50 "DimGray"))
))



(define (get-ui-pos i j)
  (list-ref (list-ref tablero i) j))

(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))

(define (mueve-ficha color i j) (
      (draw-solid-ellipse ventana1) (make-posn (list-ref (get-ui-pos i j) 0) (list-ref (get-ui-pos i j) 1) ) 50 50 color) )


(write (get-ui-pos 0 0))



(mueve-ficha "blue" 2 1)
