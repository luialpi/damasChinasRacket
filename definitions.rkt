#lang racket

;; Create the initial board
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

(define board-win-1 (list
                    (list 2 2 2 2 0 0 0 0 0 0)
                    (list 2 2 2 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 0)
                    (list 0 0 0 2 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 0)
                    (list 0 0 2 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 1)
                    (list 0 0 0 0 2 0 0 0 1 1)
                    (list 0 0 0 0 0 0 0 1 1 1)
                    (list 0 0 0 0 0 0 1 1 1 1)
               )
  )

(define board-win-2 (list
                    (list 2 2 2 2 0 1 0 1 0 1)
                    (list 2 2 2 0 0 0 1 0 0 0)
                    (list 2 2 0 0 0 0 0 0 0 0)
                    (list 2 0 1 1 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 0)
                    (list 0 0 1 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 1)
                    (list 0 0 0 0 1 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 1 1 0 0)
               )
  )

(define win-1-positions '((6 9) (7 8) (7 9) (8 7) (8 8) (8 9) (9 6) (9 7) (9 8) (9 9)))
(define win-2-positions '((0 0) (0 1) (0 2) (0 3) (1 0) (1 1) (1 2) (2 0) (2 1) (3 0)))

;; Get value of matrix in position (i j)
(define (get-value matrix i j)
  (list-ref (list-ref matrix i) j)
  )

;; Set new value in position (i j)
;; return the new matrix
(define (set-value matrix i j value)
  (set! matrix (list-set matrix i (list-set (list-ref matrix i) j value)))
  matrix
  )

;; Get positions by player
(define (get-pos-by-pl player matrix)
  (get-positions-aux matrix (range 10) player) 
)

;;Aux method to get the positions by player
(define (get-positions-aux matrix indexes player)
  (if (empty? (rest matrix))
      (add-row-index (first indexes) (indexes-of (first matrix) player))
      (append
       (add-row-index (first indexes) (indexes-of (first matrix) player))
       (get-positions-aux (rest matrix) (rest indexes) player)
       )
  )
)

;; Add the a number to each element of the parameter list
;; if row = (1 2 3) and row-index = 9 result is ((9 1), (9 2), (9 3))
;; if row is empty do nothing
(define (add-row-index row-index row)
  (if (empty? list)
      null
      (map (lambda (i) (list row-index i)) row)
      )
  )

;; Check if the given player has win or not
(define (win-check player matrix)
  (if (eq? player 1)
      (equal? (get-pos-by-pl player matrix) win-1-positions)
      (equal? (get-pos-by-pl player matrix) win-2-positions)   
      )
  )
 
;; Get all posible jumping moves from up to down for a given position i j
(define (try-jump-up-down i j matrix)
  (if (or (or (> i 9) (> j 9)) (not (eq? (get-value matrix i j) 0)))
      null
      (cond
        [(and (and (<= (+ i 1) 9) (eq? (get-value matrix (+ i 1) j) 0))
              (and (<= (+ j 1) 9) (eq? (get-value matrix i (+ j 1)) 0)))
          (list (list i j))]
        [(and (and (<= (+ i 1) 9)
                  (eq? (get-value matrix (+ i 1) j) 0))
              (and (<= (+ j 1) 9)
                   (not (eq? (get-value matrix i (+ j 1)) 0))))
          (append (list (list i j)) (try-jump-up-down i (+ j 2) matrix))]
        [(and (and (<= (+ j 1) 9)
                  (eq? (get-value matrix i (+ j 1)) 0))
              (and (<= (+ i 1) 9)
                   (not (eq? (get-value matrix (+ i 1) j) 0))))
          (append (list (list i j)) (try-jump-up-down (+ i 2) j matrix))]
        [(and (and (<= (+ i 1) 9) (<= (+ j 1) 9))
              (and (not (eq? (get-value matrix (+ i 1) j) 0)) (not (eq? (get-value matrix i (+ j 1)) 0))))
         (append (list (list i j)) (try-jump-up-down (+ i 2) j matrix) (try-jump-up-down i (+ j 2) matrix))]
        [else null]
        )
      )
  )

;; Get a list of all posible moves for a given position i j from up to down
(define (get-moves-by-position-up-down i j matrix)
  (cond
    [(and (> (+ i 1) 9) (> (+ j 1) 9)) null]
    [(and (> (+ j 1) 9) (eq? (get-value matrix (+ i 1) j) 0)) (list (+ i 1) j)]
    [(and (> (+ i 1) 9) (eq? (get-value matrix i (+ j 1)) 0)) (list i (+ j 1))]
    [else
     (if (and (<= (+ j 1) 9) (<= (+ i 1) 9))
         (cond
           [(and (eq? (get-value matrix i (+ j 1)) 0)
                 (eq? (get-value matrix (+ i 1) j) 0))
            (list (list i (+ j 1)) (list (+ i 1) j))]
           [(and (not (eq? (get-value matrix (+ i 1) j) 0))
                 (not (eq? (get-value matrix i (+ j 1)) 0)))
            (append (try-jump-up-down (+ i 2) j matrix) (try-jump-up-down i (+ j 2) matrix))]
           [(and (eq? (get-value matrix (+ i 1) j) 0) 
                 (not (eq? (get-value matrix i (+ j 1)) 0)))
            (append (list (list (+ i 1) j)) (try-jump-up-down i (+ j 2) matrix))]
           [(and (not (eq? (get-value matrix (+ i 1) j) 0))
                 (eq? (get-value matrix i (+ j 1)) 0))
            (append (try-jump-up-down (+ i 2) j matrix) (list (list i (+ j 1))))]
           [else null]
           )
         null)
     ]
    )
  )

;; Get all posible jumping moves from down to up for a given position i j
(define (try-jump-down-up i j matrix)
  (if (or (or (< i 0) (< j 0)) (not (eq? (get-value matrix i j) 0)))
      null
      (cond
        [(and (and (>= (- i 1) 0) (eq? (get-value matrix (- i 1) j) 0))
              (and (>= (- j 1) 0) (eq? (get-value matrix i (- j 1)) 0)))
          (list (list i j))]
        [(and (and (>= (- i 1) 0)
                  (eq? (get-value matrix (- i 1) j) 0))
              (and (>= (- j 1) 0)
                   (not (eq? (get-value matrix i (- j 1)) 0))))
          (append (list (list i j)) (try-jump-down-up i (- j 2) matrix))]
        [(and (and (>= (- j 1) 0)
                  (eq? (get-value matrix i (- j 1)) 0))
              (and (>= (- i 1) 0)
                   (not (eq? (get-value matrix (- i 1) j) 0))))
          (append (list (list i j)) (try-jump-down-up (- i 2) j matrix))]
        [(and (and (>= (- i 1) 0) (>= (- j 1) 0))
              (and (not (eq? (get-value matrix (- i 1) j) 0)) (not (eq? (get-value matrix i (- j 1)) 0))))
         (append (list (list i j)) (try-jump-down-up (- i 2) j matrix) (try-jump-down-up i (- j 2) matrix))]
        [else null]
        )
      )
  )
;; Get a list of all posible moves for a given position i j from down to up
(define (get-moves-by-position-down-up i j matrix)
  (cond
    [(and (< (- i 1) 0) (< (- j 1) 0)) null]
    [(and (< (- j 1) 0) (eq? (get-value matrix (- i 1) j) 0)) (list (- i 1) j)]
    [(and (< (- i 1) 0) (eq? (get-value matrix i (- j 1)) 0)) (list i (- j 1))]
    [else
     (if (and (>= (- j 1) 0) (>= (- i 1) 0))
         (cond
           [(and (eq? (get-value matrix i (- j 1)) 0)
                 (eq? (get-value matrix (- i 1) j) 0))
            (list (list i (- j 1)) (list (- i 1) j))]
           [(and (not (eq? (get-value matrix (- i 1) j) 0))
                 (not (eq? (get-value matrix i (- j 1)) 0)))
            (append (try-jump-down-up (- i 2) j matrix) (try-jump-down-up i (- j 2) matrix))]
           [(and (eq? (get-value matrix (- i 1) j) 0)
                 (not (eq? (get-value matrix i (- j 1)) 0)))
            (append (list (list (- i 1) j)) (try-jump-down-up i (- j 2) matrix))]
           [(and (not (eq? (get-value matrix (- i 1) j) 0))
                 (eq? (get-value matrix i (- j 1)) 0))
            (append (try-jump-down-up (- i 2) j matrix) (list (list i (- j 1))))]
           [else null]
           )
         null
         )
     ]
    )
  )

;; Get all posible valid moves for a list of positions
(define (get-moves-for-list player positions-list matrix)
  (if (empty? positions-list)
      null
      (if (eq? player 1)
          (if (empty? (get-moves-by-position-up-down (first (car positions-list))
                                                         (second (car positions-list))
                                                         matrix))
              (get-moves-for-list player (rest positions-list) matrix)
              (append (list (append (list (car positions-list))
                         (get-moves-by-position-up-down (first (car positions-list))
                                                         (second (car positions-list))
                                                         matrix)))
                  (get-moves-for-list player (rest positions-list) matrix)))
          (if (empty? (get-moves-by-position-down-up (first (car positions-list))
                                                         (second (car positions-list))
                                                         matrix))
               (get-moves-for-list player (rest positions-list) matrix)
              (append (list (append (list (car positions-list))
                          (get-moves-by-position-down-up (first (car positions-list))
                                                         (second (car positions-list))
                                                         matrix)))
                  (get-moves-for-list player (rest positions-list) matrix)))
          ))
  )

;;(get-moves-for-list 1 (get-pos-by-pl 1 board) board)

;; Get all posible valid moves for a given player
(define (get-moves player matrix)
  (get-moves-for-list player (get-pos-by-pl player matrix) matrix)
)

;; Alfa-Beta with minimax algoritm 
(define (h-minimax matrix deep a b player)
  (if (cut-off-test matrix deep)
      1 ;cambiar
      0 ;cambiar
      )
  )

;; Check if one of the players win or if the given deep is 0
(define (cut-off-test matrix deep)
  (or (eq? deep 0) (or (win-check 1 matrix) (win-check 2 matrix)))
 )

; cambiar
(define (eval) 0)