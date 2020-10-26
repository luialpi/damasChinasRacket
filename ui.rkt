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
(define board (build-matrix 10 10 (lambda (x y) (* x 0))))


; —————————————————————————————————
; game logic sention
; —————————————————————————————————
;check if we have a chip on i j
(define (check-posicion i j) (= (matrix-ref board i j) 0))

;;puts a chip on the position i j 
;;checks if we have a chip on that place
(define (set-posicion i j ficha)
  (cond
    [(check-posicion i j) (set! board (matrix-set board i j ficha))]
    [else (print "No se puede mover, ya hay una ficha ahi.")]
 ))

;;checks if the distance between two movements is valid
(define (check-distance origin destination)
  (define x (abs (- (list-ref origin 0) (list-ref destination 0))))
  (define y (abs (- (list-ref origin 1) (list-ref destination 1))))
  (or (= x 2) (= x 1) (= y 2) (= y 1))
)

;;validates if movement is correct
;returns true or false
(define (check-movement origin destination)
  (cond
    []
    [(not (and (list? origin) (list? destination))) false ]
    [ (or (check-posicion (list-ref origin 0) (list-ref origin 1)) (not (check-posicion (list-ref destination 0) (list-ref destination 1)))) false]
    [else
     ;;check the jumps
     (cond
       ;;check position with destination(1) +1
       [(and (> (list-ref origin 1) (list-ref destination 1)) (check-distance origin  destination))
        (not (check-posicion (list-ref destination 0) (+ 1 (list-ref destination 1)))) ]

       ;;check position with destination(0) +1
       [(and (> (list-ref origin 0) (list-ref destination 0)) (check-distance origin  destination))
        (not (check-posicion (+ 1 (list-ref destination 0)) (list-ref destination 1)))]

       ;;check position with origin(1) +1
       [(and (< (list-ref origin 1) (list-ref destination 1)) (check-distance destination origin))
        (not (check-posicion (list-ref origin 0) (+ 1 (list-ref origin 1))))]

       ;;check position with origin(1) +1
       [(and (< (list-ref origin 0) (list-ref destination 0)) (check-distance destination  origin))
        (not (check-posicion (+ 1 (list-ref destination 0)) (list-ref destination 1)))]

       ;else, if we dont have a jump then its a single movement so we procced to check it
       [else (and (check-posicion (list-ref destination 0) (list-ref destination 1)) (check-distance origin destination)) ]
       )
     ]
    )
  )

;;runs a list checking if each movement is valid, example of list: '( '(i j) '(i j) '(i j))
;;we used to check the movements of the player, cause we are already validating the movements of the ia
(define (check-movements list)
  (cond
    [(or (empty? list) (<= (length list ) 1)) true]
    [else
     ( cond
        [ (check-movement (first list) (second list)) (check-movements (cdr list))]
        [else false]
      )]
    )
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

;; initialice function, set up all the circles in the interface and also setup the initial positions in the matrix
(define iniciar-matriz
  (for ([i len] )
    ( for ([j len])
       (cond
         [(or (and (= i 0) (<= j 3)) (and (= i 1) (<= j 2)) (and (= i 2) (<= j 1)) (and (= i 3) (= j 0)))
          (set-posicion i j 1)]
         [ (or (and (= i 9) (>= j 6)) (and (= i 8) (>= j 7)) (and (= i 7) (>= j 8)) (and (= i 6) (= j 9)))
          (set-posicion i j 2)]
     ))
))

;draw all the circles 
(define dibujar-circulos
  (for ([i len] )
    ( for ([j len])
       (cond
         [(= (matrix-ref board i j) 1)
          (mueve-ficha "blue" i j)]
         [ (= (matrix-ref board i j) 2)
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

(let loop ()
  (display "Input: ")
  (define a (string-split (read-line (current-input-port) 'any)))
  (if (check-movements (split-every-two (to-int a)))
      (print "movement valid")
      (print "movement invalid")
  )
  (set! board (make-matrix 10 10 (get-best-move board 1 4)))
  (loop))
