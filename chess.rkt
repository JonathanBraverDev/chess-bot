#lang racket


(define B1 '((BR BH BB BQ BK BB BH BR)
             (BP BP BP BP BP BP BP BP)
             (-- -- -- -- -- -- -- --)
             (-- -- -- -- -- -- -- --)
             (-- -- -- -- -- -- -- --)
             (-- -- -- -- -- -- -- --)
             (WP WP WP WP WP WP WP WP)
             (WR WH WB WQ WK WB WH WR)))

;legend:
;king => K
;queen => Q
;rook => R
;khight => H
;bishop => B
;pawn => P
;enpty space\tile => --

;prefix:
;black piece => B
;white piece => W


;movement checks
(define (emptyTileAt? B Xpos Ypos)
  (equal? (tileAt B Xpos Ypos) '-))

(define (legalTile? B Xpos Ypos)
  (and (< Xpos 8) (< Ypos 8) (> Xpos -1) (> Ypos -1)))

;printing
(define (printBoard B) ;prints the board
  (cond
    ((empty? (rest B)) (print (first B)))
    (else (print (first B))
          (newline)
          (printBoard (rest B)))))

;board operations
(define (updateBoard B Xpos Ypos input)
  (cond
    ((= Ypos 0) (cons (updateCol (first B) Xpos input) (rest B)))
    (else (cons (first B) (updateBoard (rest B) Xpos (sub1 Ypos) input)))))

(define (updateCol L Xpos input)
  (cond
    ((= Xpos 0) (cons input (rest L)))
    (else (cons (first L) (updateCol (rest L) (sub1 Xpos) input)))))

(define (ClearTileAt B Xpos Ypos)
  (cond
    ((legalTile? B Xpos Ypos) (updateBoard B Xpos Ypos '-))
    (else (print '(invalid tile)))))

(define (tileAt B Xpos Ypos) ;returnes a tile in a given location
  (list-ref (list-ref B Ypos) Xpos))

