#lang racket

; —————————————————————————————————
; section de imports y variables
; —————————————————————————————————
(require "definitions.rkt")
(require graphics/graphics)
(require htdp/matrix)
(open-graphics)

;;window
(define alto 1000)
(define ancho 1200)
(define ventana1 (open-viewport "Damas Chinas" alto ancho))

;;we use it to know the len of the matrix
(define len '(0 1 2 3 4 5 6 7 8 9))

;game board
(define board (list (list 1 1 1 1 0 0 0 0 0 0)
                    (list 1 1 1 0 0 0 0 0 0 0)
                    (list 1 1 0 0 0 0 0 0 0 0)
                    (list 1 0 0 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 2)
                    (list 0 0 0 0 0 0 0 0 2 2)
                    (list 0 0 0 0 0 0 0 2 2 2)
                    (list 0 0 0 0 0 0 2 2 2 2)
               )
  )


; —————————————————————————————————
; game logic sention
; —————————————————————————————————
;check if we have a chip on i j
(define (check-posicion i j) (eq? (list-ref (list-ref board i) j) 0))

;;puts a chip on the position i j 
(define (set-value matrix i j value)
  (set! matrix (list-set matrix i (list-set (list-ref matrix i) j value)))
  matrix
 )

;;checks if the distance between two movements is valid
(define (check-distance origin destination)
  (define x (abs (- (list-ref origin 0) (list-ref destination 0))))
  (define y (abs (- (list-ref origin 1) (list-ref destination 1))))
  (or (= x 2) (= x 1) (= y 2) (= y 1))
)
(define (validade-entry x y)
  ( or (or (> x 9) (< x 0)) (or (> y 9) (< y 0))))


;;runs a list checking if each movement is valid, example of list: '( '(i j) '(i j) '(i j))
;;we used to check the movements of the player, cause we are already validating the movements of the ia
(define (check-movement list)
  (define posible-moves (get-moves 2 board))
  (print posible-moves)
  )



; —————————————————————————————————
; graphic interface section
; —————————————————————————————————

;returns the place in the board to draw a circle for each ij in the matrix
(define (get-ui-pos i j)
  (list (+ 480 (- (* 50 j) (* 50 i))) (+ 25 (+ (* 50 j) (* 50 i)))))

;draw a solid cirlce of color
(define (mueve-ficha color i j) (
  (draw-solid-ellipse ventana1) (make-posn (list-ref (get-ui-pos i j) 0) (list-ref (get-ui-pos i j) 1) ) 50 50 color))

;draw all the circles 
(define (dibujar-circulos matrix)
  (for ([i len] )
    ( for ([j len])
       (cond
         [(= (list-ref (list-ref matrix i) j) 1)
          (mueve-ficha "blue" i j)]
         [ (= (list-ref (list-ref matrix i) j) 2)
          (mueve-ficha "red" i j)]
         [else
          (mueve-ficha "DimGray" i j)
          ]
     ))
))

;;draw the numbers of the ui so the user is able to see the position of the chip
(define draw-numeros
  (for ([i len])
    ((draw-string ventana1) (make-posn (list-ref (get-ui-pos i 0) 0) (list-ref (get-ui-pos i 0) 1)) (number->string i)) 
    ((draw-string ventana1) (make-posn (+ 45 (list-ref (get-ui-pos 0 i) 0)) (- (list-ref (get-ui-pos 0 i) 1) 2)) (number->string i))
    )
)

(define (split-every-two list)
   (if (not (empty? list))
       (cons (take list 2) (split-every-two (drop list 2)))
       '() ))

(define (to-int list)
  (filter-map (lambda (x) (string->number x)) list))

(dibujar-circulos board)
(let loop ()
  ;;Human moving
  (display "Input: ")
  (define a (string-split (read-line (current-input-port) 'any)))
  (define movements (split-every-two (to-int a)))
  (cond
    [(check-movement movements)
      (set! board (set-value board (first (first movements) ) (second (first movements)) 0))
      (set! board (set-value board (first (last movements) ) (second (last movements)) 2))
     ]
    [else (print "movement invalid\n")(loop)]
  )

  ;;update board
  (dibujar-circulos board)
  
  ;;IA moving
  (set! board (get-best-move board 1 4))

  ;;Update board
  (dibujar-circulos board)
  
  (loop))
