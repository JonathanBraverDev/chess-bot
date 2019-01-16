#lang racket


(define B1 '(("WR" "WH" "WB" "WQ" "WK" "WB" "WH" "WR")
             ("WP" "WP" "WP" "WP" "WP" "WP" "WP" "WP")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("BP" "BP" "BP" "BP" "BP" "BP" "BP" "BP")
             ("BR" "BH" "BB" "BQ" "BK" "BB" "BH" "BR")))

;legend:
;king => K
;queen => Q
;rook => R
;khight => H
;bishop => B
;pawn => P
;enpty space\tile => --

;prefix:
;black piece => "B"
;white piece => "W" 

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

;pawn section (its the only piece that gets one...)
                       ;WIP
(define (pawnMoves-startingLane B Xpos Ypos side) ;side will invert the movement of the pawn (its the color...)
  (cond
    ((= 1 side) (take (lookLine B Xpos Ypos) 2))  ;white pawn move
    (else (take (lookLine B Xpos Ypos 0 -1) 2)))) ;black pawn move
    ;need to add kills (they are diffrent from regular move)

;regular move - 1 tile
;kills 1 diagonal left or rigth from movement direction
;first move - 2 tiles
;capture during first move
;croned at the end of the board
    

;move options (NO KILLS... yet ;))
(define (lookLine B Xpos Ypos [Xchange 0] [Ychange 1] [ignoreTile #T]) ;ONLY one of the cnages must be active
  (cond                                                                ;defult is WHITE pawn movement (-1 for black pawn)
    (ignoreTile (lookLine B (+ Xpos Xchange) (+ Ypos Ychange) Xchange Ychange #F)) ;to ingore the origin location
    ((IllLegalMove? B Xpos Ypos) '())
    (else (cons (list Xpos Ypos) (lookLine B (+ Xpos Xchange) (+ Ypos Ychange) Xchange Ychange #F)))))

(define (lookDiagonal B Xpos Ypos [Xchange 1] [Ychange 1] [ignoreTile #T]) ;the cnages are 1 or -1
  (cond                                                                    ;defult is bottom right diagonal
    (ignoreTile (lookDiagonal B (+ Xpos Xchange) (+ Ypos Ychange) Xchange Ychange #F))
    ((IllLegalMove? B Xpos Ypos) '())
    (else (cons (list Xpos Ypos) (lookDiagonal B (+ Xpos Xchange) (+ Ypos Ychange) Xchange Ychange #F)))))


;movement checks
(define (IllLegalMove? B Xpos Ypos)
  (or (not (legalTile? B Xpos Ypos)) (not (emptyTile? B Xpos Ypos))))

(define (emptyTile? B Xpos Ypos)
  (equal? (getTileAt B Xpos Ypos) "--"))

(define (legalTile? B Xpos Ypos)
  (and (< Xpos 8) (< Ypos 8) (> Xpos -1) (> Ypos -1)))

(define (getColor B Xpos Ypos)
  (string-ref (getTileAt B Xpos Ypos) 0))

;printing
(define (printBoard B) ;prints the board
  (for-each (lambda (line) (for-each (lambda (x) (display x) (display " ")) line) (newline)) B))
   

;board operations
(define (findPosOfAll B target Xpos Ypos)
  (cond
    ((= Ypos 8) '())
    ((= Xpos 8) (findPosOfAll B target 0 (add1 Ypos)))
    ((equal? (getTileAt B Xpos Ypos) target) (cons (list Xpos Ypos) (findPosOfAll B target (add1 Xpos) Ypos)))
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

(define (getTileAt B Xpos Ypos) ;returnes a tile in a given location
  (list-ref (list-ref B Ypos) Xpos))
