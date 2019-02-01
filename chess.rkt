#lang racket
(require racket/list)


(define B1 '(("WR" "WH" "WB" "WQ" "WK" "WB" "WH" "WR")
             ("WP" "WP" "WP" "WP" "WP" "WP" "WP" "WP")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("BP" "BP" "BP" "BP" "BP" "BP" "BP" "BP")
             ("BR" "BH" "BB" "BQ" "BK" "BB" "BH" "BR")))

(define B2 '(("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "WH" "--" "--" "--" "--")
             ("--" "--" "BH" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "BQ" "--" "--" "--")
             ("--" "--" "WH" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")))

(define TST '(("WH" "--")
              ("--" "--")))

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
;           (possibleKingMoves B color))) ;!need to add the origin location to every move list!;

(define (possiblePawnMoves B color)
  (append (map (findPosOfAll B1 'BP 0 0))))

;pawn section (its the only piece that gets one...)

(define (PawnPossibleMoves B Xpos Ypos [color (getColor B Xpos Ypos)])
  (cons (list Xpos Ypos) '())) ;will take all moves in here

(define (pawnMoves-startingLane B Xpos Ypos side) ;side will invert the movement of the pawn (its the color...)
  (cond
    ((= 1 side) (take (lookLine B Xpos Ypos) 2))  ;white pawn move ;wont work, missing color parameter
    (else (take (lookLine B Xpos Ypos 0 -1) 2)))) ;black pawn move ;wont work, missing color parameter
    ;need to add kills (they are diffrent from regular move)

;(define (pawnMoves-regularKills B Xpos Ypos side) ;WIP
;  (cond
;    ()))


;regular move - 1 tile
;kills 1 diagonal left or rigth from movement direction
;first move - 2 tiles
;capture during first move
;croned at the end of the board


;Knight movement ;WORKING
(define (KnightPossibleMoves B Xpos Ypos [color (getColor B Xpos Ypos)])
  (cons (list Xpos Ypos) (Knight-addPossibleMovesFromList B Xpos Ypos (list '(2 -1) '(-2 -1)
                                                                            '(2 1)  '(-2 1)
                                                                            '(1 -2) '(-1 -2)
                                                                            '(1 2)  '(-1 2)) color)))

(define (Knight-addPossibleMovesFromList B originX originY L originColor) ;list is moves relative to the origin location
  (cond                                                                    
    ((empty? L) '())
    ((not (LegalMove? B (+ (first (first L)) originX) (+ (second (first L)) originY) originColor)) (Knight-addPossibleMovesFromList B originX originY (rest L) originColor))
    (else (cons (list (+ (first (first L)) originX) (+ (second (first L)) originY)) (Knight-addPossibleMovesFromList B originX originY (rest L) originColor)))))

;(define (KnightJumps-sides Xpos Ypos [Xchange -2] [Ychange -1] [ignoreTile #T] [changedir #F]) ;hardcoded... (and obviusly not done)
;  (cond
;    (ignoreTile (KnightJumps-sides (+ Xpos Xchange) (+ Ypos Ychange) Xchange Ychange #F))
;    (changedir 
;    ((not (LegalMove? B Xpos Ypos)) '())
;    (else (cons (list Xpos Ypos) (KnightJumps-sides (+ Xpos Xchange) (+ Ypos Ychange) Xchange Ychange #F)

;rook movement (EZ) ;WORKING
(define (RookPossibleMoves B Xpos Ypos)
  (cons (list Xpos Ypos) ;first enelemt is the origin location
        (flatten-lists
         (removeAllOccurrencesOf '()
          (let ([color (getColor B Xpos Ypos)])
            (list (lookLine B Xpos Ypos color) ;will run defult (down)
                  (lookLine B Xpos Ypos color 0 -1) ;up
                  (lookLine B Xpos Ypos color 1 0) ;right
                  (lookLine B Xpos Ypos color -1 0))))))) ;left

;bishop movement (EZ) ;WORKING
(define (BishopPossibleMoves B Xpos Ypos)
  (cons (list Xpos Ypos) ;first enelemt is the origin location
        (flatten-lists
         (removeAllOccurrencesOf '()
          (let ([color (getColor B Xpos Ypos)])
            (list (lookDiagonal B Xpos Ypos color) ;defult (bottom right)
                  (lookDiagonal B Xpos Ypos color 1 -1) ;upper right
                  (lookDiagonal B Xpos Ypos color -1 1) ;bottom left
                  (lookDiagonal B Xpos Ypos color -1 -1))))))) ;upper left

;queen movement (SOOOO EZ) ;WORKING
(define (QueenPossibleMoves B Xpos Ypos)
  (cons (list Xpos Ypos)
        (removeAllOccurrencesOf '() (append (rest (RookPossibleMoves B Xpos Ypos))
                                            (rest (BishopPossibleMoves B Xpos Ypos))))))

;king movement
(define (KingPossibleMoves B Xpos Ypos)
  (cons (list Xpos Ypos)
        (let ([color (getColor B Xpos Ypos)])
          (King-addPossibleMovesFromList B Xpos Ypos (list '(1 -1) '(1 0) '(1 1) '(0 1)
                                                           '(0 -1) '(-1 -1) '(-1 0) '(-1 1)) (getColor B Xpos Ypos)))))

(define (King-addPossibleMovesFromList B originX originY L originColor)
  (cond 
    ((empty? L) '())
    ((not (LegalMove? B (+ (first (first L)) originX) (+ (second (first L)) originY) originColor)) (King-addPossibleMovesFromList B originX originY (rest L) originColor)) 
    ((attackedTile? B (+ (first (first L)) originX) (+ (second (first L)) originY) originColor) (King-addPossibleMovesFromList B originX originY (rest L) originColor)) ;filteres out attacked tiles
    (else (cons (list (+ (first (first L)) originX) (+ (second (first L)) originY)) (King-addPossibleMovesFromList B originX originY (rest L) originColor)))))

(define (attackedTile? B Xpos Ypos)
  (cond))


(define (attackedByKnight B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (KnightPossibleMoves B Xpos Ypos targetColor))])
  (println L)
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter) ;it wont ger here if ATKCounter is at 0
    ((and (isKnight? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByKnight B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (println 'else) (attackedByKnight B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isKnight? B Xpos Ypos)
  (cond
    ((equal? (getType B Xpos Ypos) #\H) #T)
    (else #F)))


;move options
(define (lookLine B Xpos Ypos color [Xchange 0] [Ychange 1] [ignoreTile #T]) ;ONLY one of the cnages must be active
  (cond                                                                ;defult is WHITE pawn movement (-1 for black pawn)
    (ignoreTile (lookLine B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F)) ;to ingore the origin location
    ((not (LegalMove? B Xpos Ypos color)) '())
    (else (cons (list Xpos Ypos) (lookLine B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F)))))

(define (lookDiagonal B Xpos Ypos color [Xchange 1] [Ychange 1] [ignoreTile #T]) ;the cnages are 1 or -1
  (cond                                                                    ;defult is bottom right diagonal
    (ignoreTile (lookDiagonal B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F))
    ((not (LegalMove? B Xpos Ypos color)) '())
    (else (cons (list Xpos Ypos) (lookDiagonal B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F)))))



;movement checks
(define (LegalMove? B Xpos Ypos color) ;color of origin piece, xy of target
  (cond
    ((and (legalTile? B Xpos Ypos) (not (friendlyTile? B Xpos Ypos color))) #T)
    (else #F)))

(define (friendlyTile? B Xpos Ypos color) ;color of origin piece, xy of target
  (cond
    ((equal? color (getColor B Xpos Ypos)) #T)
    (else #F)))

(define (legalTile? B Xpos Ypos)
  (and (< Xpos (length (first B))) (< Ypos (length B)) (> Xpos -1) (> Ypos -1)))

(define (getColor B Xpos Ypos)
  (string-ref (getTileAt B Xpos Ypos) 0))

(define (getType B Xpos Ypos)
  (string-ref (getTileAt B Xpos Ypos) 1))

(define (kill? B Xtarget Ytarget attackerColor) ;legal tiles assumed, its an inner function
  (cond
    ((or (not (equal? (getTileAt B Xtarget Ytarget) "--")) (equal? (getColor B Xtarget Ytarget) attackerColor)) #F)
    (else #T)))

;printing
(define (printBoard B) ;prints the board
  (for-each displayln B))


;main
(define (PVP B [color #\W])
  (cond
    ((equal? color #\W) (displayln "white's turn"))
    (else (displayln "black's turn")))
  (PVP (selectTile B color) (invertColor color)))

(define (selectTile B [color #\W])
  (printBoard B)
  (displayln "enter X of tile (up to 7)")
  (define Xpos (read))
  (displayln "enter Y of tile (up tp 7)")
  (define Ypos (read))
  (moveOptions B Xpos Ypos color))
   
(define (moveOptions B Xpos Ypos color)
  (cond
    ((not (equal? (getColor B Xpos Ypos) color)) (displayln "pick your own piece") (newline) (selectTile B color))
    (else (selectMove B (possibleMovesForTile B Xpos Ypos) color))))

(define (possibleMovesForTile B Xpos Ypos [target (getType B Xpos Ypos)])
  (cond
    ((equal? target #\P) (PawnPossibleMoves B Xpos Ypos))
    ((equal? target #\B) (BishopPossibleMoves B Xpos Ypos))
    ((equal? target #\H) (KnightPossibleMoves B Xpos Ypos))
    ((equal? target #\R) (RookPossibleMoves B Xpos Ypos))
    ((equal? target #\Q) (QueenPossibleMoves B Xpos Ypos))
    ((equal? target #\K) (KingPossibleMoves B Xpos Ypos))
    (else 'ERR-cant-recognize-piece)))

(define (selectMove B movesL color)
  (cond
    ((empty? (rest movesL)) (displayln "can't move") (selectTile B color))
    (else 
     (displayln "pick a move (index):")
     (displayln (rest movesL)) ;to ignore origin location
     (newline)
     (define moveIndex (add1 (read)))
     (moveTo B (first (first movesL)) (second (first movesL)) (first (list-ref movesL moveIndex)) (second (list-ref movesL moveIndex))))))
   

;board operations
(define (findPosOfAll B target Xpos Ypos)
  (cond
    ((= Ypos (length B)) '())
    ((= Xpos (length (first B))) (findPosOfAll B target 0 (add1 Ypos)))
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
    ((legalTile? B Xpos Ypos) (updateBoard B Xpos Ypos "--"))
    (else (print '(invalid tile)))))

(define (getTileAt B Xpos Ypos) ;returnes a tile in a given location
  (list-ref (list-ref B Ypos) Xpos))

;general use
(define (removeAllOccurrencesOf target L)
  (cond
    ((empty? L) '())
    ((equal? target (first L)) (removeAllOccurrencesOf target (rest L)))
    (else (cons (first L) (removeAllOccurrencesOf target (rest L))))))

(define (moveTo B Xorigin Yorigin Xtarget Ytarget)
  (clearTileAt (updateBoard B Xtarget Ytarget (getTileAt B Xorigin Yorigin)) Xorigin Yorigin))

(define (flatten-lists L)
  (pairify (flatten L))) ;not my naming idea...

(define (pairify L) ;the length of the list is always devisible by 2 (each list contains 2 numbers)
  (cond
    ((empty? L) '())
    (else (cons (list (first L) (second L)) (pairify (drop L 2))))))

(define (invertColor color)
  (cond
    ((equal? color #\W) #\B)
    (else #\W)))
  
