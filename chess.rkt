#lang racket


(define B1 '((WR WH WB WQ WK WB WH WR)
             (WP WP WP WP WP WP WP WP)
             (-- -- -- -- -- -- -- --)
             (-- -- -- -- -- -- -- --)
             (-- -- -- -- -- -- -- --)
             (-- -- -- -- -- -- -- --)
             (BP BP BP BP BP BP BP BP)
             (BR BH BB BQ BK BB BH BR)))

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

;movement

;(define (possibleMoves B color)
;  (append (possiblePawnMoves B color)
;           (possibleBishopMoves B color)
;           (possibleKhightMoves B color)
;           (possibleRookMoves B color)
;           (possibleQueenMoves B color)
;           (possibleKingMoves B color)))

(define (possiblePawnMoves B color)
  (append (map (findPosOfAll B1 'BP 0 0))))

(define (pawnMoves B Xpos Ypos side) ;side will invert the movement (its the color...)
  (cond))


(define (lookUp B Xpos Ypos [maxRange 8]) ;negative tiles will cause it to look down ;)
  (cond
    ((zero? maxRange) '())
    ((or (not (legalTile? Xpos Ypos)) (not (emptyTile? Xpos Ypos))) '())
    (else (cons (list Xpos Ypos) (lookUp B (add1 Xpos)Ypos [maxRange 8])))))


;movement checks
(define (emptyTile? B Xpos Ypos)
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
(define (findPosOfAll B target Xpos Ypos)
  (cond
    ((= Ypos 8) '())
    ((= Xpos 8) (findPosOfAll B target 0 (add1 Ypos)))
    ((equal? (tileAt B Xpos Ypos) target) (cons (list Xpos Ypos) (findPosOfAll B target (add1 Xpos) Ypos)))
    (else (findPosOfAll B target (add1 Xpos) Ypos))))

(define (updateBoard B Xpos Ypos input)
  (cond
    ((= Ypos 0) (cons (updateCol (first B) Xpos input) (rest B)))
    (else (cons (first B) (updateBoard (rest B) Xpos (sub1 Ypos) input)))))

(define (updateCol L Xpos input)
  (cond
    ((= Xpos 0) (cons input (rest L)))
    (else (cons (first L) (updateCol (rest L) (sub1 Xpos) input)))))

(define (clearTileAt B Xpos Ypos)
  (cond
    ((legalTile? B Xpos Ypos) (updateBoard B Xpos Ypos '-))
    (else (print '(invalid tile)))))

(define (tileAt B Xpos Ypos) ;returnes a tile in a given location
  (list-ref (list-ref B Ypos) Xpos))

