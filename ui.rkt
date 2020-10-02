#lang racket

(require 2htdp/image)
(require graphics/graphics)
(open-graphics)

(define alto 1000)
(define cir-pos-inicial ( - ( / alto 2 ) 50 ))
(define ventana1 (open-viewport "AJEDREZ" alto alto))
(define posiciones-ui
  (list (list '(10 450))
        (list '(60 400) '(60 500) )
        (list '(110 350) '(110 450) '(110 550) )
        (list '(160 300) '(160 400) '(160 500) '(160 600) )
        (list '(210 250) '(210 350) '(210 450) '(210 550) '(210 650) )
        (list '(260 200) '(260 300) '(260 400) '(260 500) '(260 600) '(260 700) )
        (list '(310 150) '(310 250) '(310 350) '(310 450) '(310 550) '(310 650) '(310 750) )
        (list '(360 100) '(360 200) '(360 300) '(360 400) '(360 500) '(360 600) '(360 700) '(360 800) )
        (list '(410 50) '(410 150) '(410 250) '(410 350) '(410 450) '(410 550) '(410 650) '(410 750) '(410 850) )
        (list '(460 0) '(460 100) '(460 200) '(460 300) '(460 400) '(460 500) '(460 600) '(460 700) '(460 800) '(460 900) )
        (list '(510 50) '(510 150) '(510 250) '(510 350) '(510 450) '(510 550) '(510 650) '(510 750) '(510 850) )
        (list '(560 100) '(560 200) '(560 300) '(560 400) '(560 500) '(560 600) '(560 700) '(560 800) )
        (list '(610 150) '(610 250) '(610 350) '(610 450) '(610 550) '(610 650) '(610 750) )
        (list '(660 200) '(660 300) '(660 400) '(660 500) '(660 600) '(660 700) )
        (list '(710 250) '(710 350) '(710 450) '(710 550) '(710 650) )
        (list '(760 300) '(760 400) '(760 500) '(760 600) )
        (list '(810 350) '(810 450) '(810 550) )
        (list '(860 400) '(860 500) )
        (list '(910 450) )
        ))


(define dibujar-circulos
  (for ([i posiciones-ui] )
    ( for ([j i])
     ((draw-solid-ellipse ventana1) (make-posn (list-ref j 0) (list-ref j 1) ) 50 50 "DimGray"))
))


(write (list-ref (list-ref posiciones-ui 1) 1))
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 0) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 0) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 1) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 1) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 1) 1) 0) (list-ref (list-ref (list-ref posiciones-ui 1) 1) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 2) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 2) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 2) 1) 0) (list-ref (list-ref (list-ref posiciones-ui 2) 1) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 2) 2) 0) (list-ref (list-ref (list-ref posiciones-ui 2) 2) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 1) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 1) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 2) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 2) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 3) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 3) 1) ) 50 50 "red")

((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 0) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 0) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 1) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 1) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 1) 1) 0) (list-ref (list-ref (list-ref posiciones-ui 1) 1) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 2) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 2) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 2) 1) 0) (list-ref (list-ref (list-ref posiciones-ui 2) 1) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 2) 2) 0) (list-ref (list-ref (list-ref posiciones-ui 2) 2) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 0) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 0) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 1) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 1) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 2) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 2) 1) ) 50 50 "red")
((draw-solid-ellipse ventana1) (make-posn (list-ref (list-ref (list-ref posiciones-ui 3) 3) 0) (list-ref (list-ref (list-ref posiciones-ui 3) 3) 1) ) 50 50 "red")






