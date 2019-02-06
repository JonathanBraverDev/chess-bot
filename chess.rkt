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

(define B2 '(("--" "WH" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "WH" "--" "--" "--" "--")
             ("--" "--" "BH" "--" "--" "WP" "--" "--")
             ("WQ" "--" "--" "--" "BQ" "--" "--" "--")
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

;value of each piece:
;Pawn - 1
;Bishop - 3
;Knight - 3
;Rook - 5
;Queen - 9
;King - game (JK... but realy, its game over if you lose him so its infinity)

;movement

;(define (possibleMoves B color) ;needs to return pairs of 2 locations: one origin and one destanation
;  (append (possiblePawnMoves B color)
;           (possibleBishopMoves B color)
;           (possibleKhightMoves B color)
;           (possibleRookMoves B color)
;           (possibleQueenMoves B color)
;           (possibleKingMoves B color)))

;(define (possiblePawnMoves B color)
;  (append (map (findPosOfAll B1 'BP 0 0))))

;pawn section (its the only piece that gets one...)

(define (PawnPossibleMoves B Xpos Ypos [color (getColor B Xpos Ypos)])
  (cons (list Xpos Ypos)
        (flatten-lists
         (removeAllOccurrencesOf '()
          (let ([side  (sideFinder color)])
            (list (pawnMoves-regularKills B Xpos Ypos side)
                  (pawnMoves-startingLane B Xpos Ypos side)
                  '())))))) ;more moves soon


(define (pawnMoves-regualarMove B Xpos Ypos side)
  (cond
    ((equal? (getTileAt B Xpos (+ Ypos side)) "--") (list Xpos (+ Ypos side))) ;WORNING! this is a single list and may fuck up other stuff
    (else '()))) ;defult 'no new move here' output that will get filtered out

(define (pawnMoves-startingLane B Xpos Ypos side [color (getColor B Xpos Ypos)]) ;side will invert the movement of the pawn (its the color...)
  (cond
    ((and (isOnStartingLane? Ypos color) (not (empty? (pawnMoves-regualarMove B Xpos Ypos side)))) (cons (pawnMoves-regualarMove B Xpos (+ Ypos side) side) (pawnMoves-regualarMove B Xpos Ypos side))) ;i check if he can move at all and then add amove IF he can go 2 tiles
    (else (pawnMoves-regualarMove B Xpos Ypos side)))) ;its just gonna do its own thing...
    

(define (originLaneFinder color) ;ummm it just tells the Y of the starting lane
  (cond
    ((equal? color #\W) 1)
    (else 6)))

(define (isOnStartingLane? Ypos color)
  (cond
    ((= Ypos (originLaneFinder color)) #T)
    (else #F)))

(define (sideFinder color) ;again, its simple... (returns the DEFULT change in Y)
  (cond
    ((equal? color #\W) 1)
    (else -1)))

(define (pawnMoves-regularKills B Xpos Ypos side [L '(1 -1)] [color (getColor B Xpos Ypos)])
  (cond
    ((empty? L) '())
    ((and (legalTile? B (+ Xpos (first L)) (+ Ypos side)) (kill? B (+ Xpos (first L)) (+ Ypos side) color)) (cons (list (+ Xpos (first L)) (+ Ypos side)) (pawnMoves-regularKills B Xpos Ypos side (rest L) color)))
    (else (pawnMoves-regularKills B Xpos Ypos side (rest L) color))))

;regular move - 1 tile                                    DONE
;kills 1 diagonal left or rigth from movement direction   DONE
;first move - 2 tiles                                     DONE
;capture during first move                                      some hard shit here, i need to save the last move
;croned at the end of the board                                 just like starting lane but with a change of the peice in the move


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

;king movement ;WIP!  DO NOT USE
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
  (cond)) ;WIP


(define (attackedByKnight B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (KnightPossibleMoves B Xpos Ypos targetColor))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter) ;it wont ger here if ATKCounter is at 0
    ((and (isKnight? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByKnight B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByKnight B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isKnight? B Xpos Ypos)
  (cond
    ((equal? (getType B Xpos Ypos) #\H) #T)
    (else #F)))

(define (attackedByBishopOrQueen B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (BishopPossibleMoves B Xpos Ypos))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter)
    ((and (isBishopOrQueen? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByBishopOrQueen B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByBishopOrQueen B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isBishopOrQueen? B Xpos Ypos)
  (cond
    ((or (equal? (getType B Xpos Ypos) #\B) (equal? (getType B Xpos Ypos) #\Q)) #T)
    (else #F)))

(define (attackedByRookOrQueen B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (RookPossibleMoves B Xpos Ypos))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter)
    ((and (isRookOrQueen? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByRookOrQueen B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByRookOrQueen B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isRookOrQueen? B Xpos Ypos)
  (cond
    ((or (equal? (getType B Xpos Ypos) #\R) (equal? (getType B Xpos Ypos) #\Q)) #T)
    (else #F)))

(define (attackedByPawn B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (list '(1 1) '(-1 1) '(1 -1) '(-1 -1))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter)
    ((and (isPawn? B (+ Xpos (first (first L))) (+ Ypos (second (first L)))) (not (friendlyTile? B (+ Xpos (first (first L))) (+ Ypos (second (first L))) targetColor))
          (isIn? (pawnMoves-regularKills B (+ Xpos (first (first L))) (+ Ypos (second (first L))) (sideFinder (invertColor targetColor))) (list Xpos Ypos))) (attackedByPawn B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByPawn B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isPawn? B Xpos Ypos)
  (cond
    ((equal? (getType B Xpos Ypos) #\P) #T)
    (else #F)))
    
;need to add pawn moves too


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
    ((equal? color (getColor B Xpos Ypos)) #T) ;ummmm MAYbe pointless?...
    (else #F)))

(define (legalTile? B Xpos Ypos)
  (and (< Xpos (length (first B))) (< Ypos (length B)) (> Xpos -1) (> Ypos -1)))

(define (getColor B Xpos Ypos)
  (string-ref (getTileAt B Xpos Ypos) 0))

(define (getType B Xpos Ypos)
  (string-ref (getTileAt B Xpos Ypos) 1))

(define (kill? B Xtarget Ytarget attackerColor) ;legal tiles assumed, its an inner function
  (cond
    ((or (equal? (getTileAt B Xtarget Ytarget) "--") (equal? (getColor B Xtarget Ytarget) attackerColor)) #F)
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
(define (findPosOfAll B target Xpos Ypos) ;will (probably) be replaced by find aoo color, it rund once over the board, not 6 times for every piece type ;)
  (cond                                   ;they're VERY similar anyway and i have no reay use for this one...
    ((= Ypos (length B)) '())
    ((= Xpos (length (first B))) (findPosOfAll B target 0 (add1 Ypos)))
    ((equal? (getTileAt B Xpos Ypos) target) (cons (list Xpos Ypos) (findPosOfAll B target (add1 Xpos) Ypos)))
    (else (findPosOfAll B target (add1 Xpos) Ypos))))

(define (findAllColor B color [Xpos 0] [Ypos 0])
  (cond
    ((= Ypos (length B)) '())
    ((= Xpos (length (first B))) (findAllColor B color 0 (add1 Ypos)))
    ((equal? (getColor B Xpos Ypos) color) (cons (list Xpos Ypos) (findAllColor B color (add1 Xpos) Ypos)))
    (else (findAllColor B color (add1 Xpos) Ypos))))

(define (isIn? L target)
  (cond
    ((empty? L) #F)
    ((equal? (first L) target) #T)
    (else (isIn? (rest L) target))))

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

;minimax base
(define (allMovesForColor B color [L (findAllColor B color)])
  (cond
    ((empty? (rest L)) (list (possibleMovesForTile B (first (first L)) (second (first L)))))
    (else (cons (possibleMovesForTile B (first (first L)) (second (first L)))  (allMovesForColor B color (rest L))))))

(define (addOriginPosToDestanation L [index 1]) ;index 0 is the origin ;) (no bugs XD)
  (cond
    ((= index (length L)) '())
    (else (cons (list (first L) (list-ref L index)) (addOriginPosToDestanation L (add1 index))))))

(define (allPossibleMovesForColor B color [L (allMovesForColor B color)])
   (cond
     ((empty? L) '())
     (else (append (addOriginPosToDestanation (first L)) (allPossibleMovesForColor B color (rest L))))))

(define (getAllMovesForColor B color) ;just removes
  (removeAllOccurrencesOf '() (allPossibleMovesForColor B color)))

(define (makeAllMoves B color [L (allPossibleMovesForColor B color)]) ;first cuse its along list
  (cond
    ((empty? (rest L)) #|there is a function for this... i'll update it|# 'done) ;temporary output ONLY
    (else (printBoard (moveTo B (first (first (first L))) (second (first (first L))) (first (second (first L))) (second (second (first L)))))
          (newline)
          (makeAllMoves B color (rest L))))) ;YASSSSSSSSSSS working

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
  
