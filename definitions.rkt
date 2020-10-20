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
  (let ([countInd 0])
  (cond [(eq? player "c") (map (lambda (row) (indexes-of row 1) ) matrix )]
        [else
         (map (lambda (row) (indexes-of row 2) ) matrix )]
      
      )
  )

(define (add-row-index row-index row)
  (if (empty? list)
      null
      (map (lambda (i) (list row-index i)) row)
      )
  )

(define (get-pos matrix)
      (map (lambda (index row) (add-row-index index row) ) (range 10) (indexes-of row 1) )

  )

