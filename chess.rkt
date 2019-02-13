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
             ("--" "--" "--" "--" "WQ" "--" "--" "WH")
             ("--" "--" "--" "--" "--" "--" "--" "WP")
             ("--" "--" "--" "--" "WR" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "WB")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")))

(define TST '(("--" "--")
              ("--" "--")))

;legend:
;king => K  
;queen => Q
;rook => R
;khight => H
;bishop => B
;pawn => P
;enpty space\tile => --

;WD - white target dummy
;BD - black target dummy

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

;pawn section (its the only piece that gets one...)
(define (PawnPossibleMoves B Xpos Ypos [color (getColor B Xpos Ypos)])
  (cons (list Xpos Ypos)
        (flatten-lists
         (removeAllOccurrencesOf '()
          (let ([side  (sideFinder color)])
            (list (pawnMoves-regularKills B Xpos Ypos side)
                  (pawnMoves-startingLane B Xpos Ypos side)  
                  '())))))) ;more moves soon
;need to add crowning, the only reason the game (yes... the one in which the kings are dead 10 turnds in...)
;crashed is cuse a pawn gets to the last lane and tries to move the next turn, getting to index 8 (out of 7) and crashing

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
;need to add a 'if king is attacked now' then 'you HAVE to move him OR kill the attacker on THIS turn' filter
;it'll leave only the moves that (looking at the board after the move and finding no attack) protects the king, or his own moves

(define (Knight-addPossibleMovesFromList B originX originY L originColor) ;list is moves relative to the origin location
  (cond                                                                    
    ((empty? L) '())
    ((not (LegalMove? B (+ (first (first L)) originX) (+ (second (first L)) originY) originColor)) (Knight-addPossibleMovesFromList B originX originY (rest L) originColor))
    (else (cons (list (+ (first (first L)) originX) (+ (second (first L)) originY)) (Knight-addPossibleMovesFromList B originX originY (rest L) originColor)))))


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

;king movement ;STILL WIP
(define (KingPossibleMoves B Xpos Ypos)
  (cons (list Xpos Ypos)
        (King-addPossibleMovesFromList (clearTileAt B Xpos Ypos) Xpos Ypos)))
                                        ;to get the king out of the way of potential attackers

(define (King-addPossibleMovesFromList B originX originY [L (list '(1 -1) '(1 0) '(1 1) '(0 1) '(0 -1) '(-1 -1) '(-1 0) '(-1 1))] [attackedColor (getColor B originX originY)])
  (cond 
    ((empty? L) '())
    ((not (LegalMove? B (+ (first (first L)) originX) (+ (second (first L)) originY) attackedColor)) (King-addPossibleMovesFromList B originX originY (rest L) attackedColor)) 
    ((attackedTile? B (+ (first (first L)) originX) (+ (second (first L)) originY) attackedColor) (King-addPossibleMovesFromList B originX originY (rest L) attackedColor)) ;filteres out attacked tiles
    (else (cons (list (+ (first (first L)) originX) (+ (second (first L)) originY)) (King-addPossibleMovesFromList B originX originY (rest L) attackedColor)))))

(define (filterOutKingDeaths B L attackedColor)
  (cond
    ((empty? L) '())
    ((attackedTile? B (first (first L)) (second (first L)) attackedColor) (filterOutKingDeaths B (rest L) attackedColor))
    (else (cons (first L) (filterOutKingDeaths B (rest L) attackedColor)))))

(define (attackedTile? B Xpos Ypos [attackedColor (getColor B Xpos Ypos)]) ;there IS A LOT of optimization to be done here... all the functions look mostly the same
  (define dummy (makeDummy attackedColor))                                 ;I'll worry about refactoring later, its modular anyway

;  (print Xpos) (println Ypos) ;for debug only (I wipe unnecesery debugs on the next update)
  (let ([newB (updateBoard B Xpos Ypos dummy)]) ;placing a target so nearby pieces will see it as a legal move
;    (println (attackedByKnight newB Xpos Ypos))
;    (println (attackedByBishopOrQueen newB Xpos Ypos))
;    (println (attackedByRookOrQueen newB Xpos Ypos))
;    (println (attackedByPawn (updateBoard newB Xpos Ypos dummy) Xpos Ypos))
;    (println (attackedByKing newB Xpos Ypos attackedColor))
;    (newline)
  
    (or
     (attackedByKnight newB Xpos Ypos)
     (attackedByBishopOrQueen newB Xpos Ypos)
     (attackedByRookOrQueen newB Xpos Ypos)
     (attackedByPawn newB Xpos Ypos)
     (attackedByKing newB Xpos Ypos attackedColor))))

(define (makeDummy dummyColor) ;its so simple that it's here just for the ease of use
  (string dummyColor #\D))
  
    
(define (attackedByKnight B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (KnightPossibleMoves B Xpos Ypos targetColor))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter) ;it wont ger here if ATKCounter is at 0
    ((and (isKnight? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByKnight B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByKnight B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isKnight? B Xpos Ypos [outOfBounds (not (legalTile? B Xpos Ypos))])
  (cond
    (outOfBounds #F)
    ((equal? (getType B Xpos Ypos) #\H) #T)
    (else #F)))

(define (attackedByBishopOrQueen B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (BishopPossibleMoves B Xpos Ypos))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter)
    ((and (isBishopOrQueen? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByBishopOrQueen B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByBishopOrQueen B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isBishopOrQueen? B Xpos Ypos [outOfBounds (not (legalTile? B Xpos Ypos))])
  (cond
    (outOfBounds #F)
    ((or (equal? (getType B Xpos Ypos) #\B) (equal? (getType B Xpos Ypos) #\Q)) #T)
    (else #F)))

(define (attackedByRookOrQueen B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (rest (RookPossibleMoves B Xpos Ypos))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter)
    ((and (isRookOrQueen? B (first (first L)) (second (first L))) (not (friendlyTile? B (first (first L)) (second (first L)) targetColor))) (attackedByRookOrQueen B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
    (else (attackedByRookOrQueen B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isRookOrQueen? B Xpos Ypos [outOfBounds (not (legalTile? B Xpos Ypos))])
  (cond
    (outOfBounds #F)
    ((or (equal? (getType B Xpos Ypos) #\R) (equal? (getType B Xpos Ypos) #\Q)) #T)
    (else #F)))

(define (attackedByPawn B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (list '(1 1) '(-1 1) '(1 -1) '(-1 -1))])
  (let ([newX (+ Xpos (first (first L)))]
        [newY (+ Ypos (second (first L)))])
    (cond ;need to update the board to have a 'dummy target' at the origin location
      ((and (empty? (rest L)) (zero? ATKCounter)) #F)
      ((empty? (rest L)) ATKCounter)
      ((and (isPawn? B newX newY) (not (friendlyTile? B newX newY targetColor))
            (isIn? (pawnMoves-regularKills B newX newY (sideFinder (invertColor targetColor))) (list Xpos Ypos))) (attackedByPawn B Xpos Ypos targetColor (add1 ATKCounter) (rest L)))
      (else (attackedByPawn B Xpos Ypos targetColor ATKCounter (rest L))))))

(define (isPawn? B Xpos Ypos [outOfBounds (not (legalTile? B Xpos Ypos))])
  (cond
    (outOfBounds #F)
    ((equal? (getType B Xpos Ypos) #\P) #T)
    (else #F)))

(define (attackedByKing B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (list '(1 -1) '(1 0) '(1 1) '(0 1) '(0 -1) '(-1 -1) '(-1 0) '(-1 1))])
  (cond
    ((and (empty? L) (zero? ATKCounter)) #F)
    ((empty? L) ATKCounter)
    ((and (isKing? B (+ Xpos (first (first L))) (+ Ypos (second (first L)))) (not (friendlyTile? B (+ Xpos (first (first L))) (+ Ypos (second (first L))) targetColor))) 1) ;there can be only one king
    (else (attackedByKing B Xpos Ypos targetColor ATKCounter (rest L)))))

(define (isKing? B Xpos Ypos [outOfBounds (not (legalTile? B Xpos Ypos))])
  (cond
    (outOfBounds #F)
    ((equal? (getType B Xpos Ypos) #\K) #T)
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

(define (PVEdemo B [color #\W] [human #T]) ;its a completly random bot
  (cond
    ((equal? color #\W) (displayln "white's turn"))
    (else (displayln "black's turn")))
  (cond
    (human (PVEdemo (selectTile B color) (invertColor color) (invertPlayer human)))
    (else (let ([move (randomIndexFrom (allPossibleMovesForColor B color))])
            (PVEdemo (makeMove B move) (invertColor color) (invertPlayer human))))))

(define (EVEbullshit B [color #\W]) ;its a completly random bot
  (printBoard B)
  (newline)
  (cond
    ((equal? color #\W) (displayln "white's turn"))
    (else (displayln "black's turn")))
  (let ([move (randomIndexFrom (allPossibleMovesForColor B color))])
    (EVEbullshit (makeMove B move) (invertColor color))))
   

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

(define (makeMove B L)
  (moveTo B (first (first L)) (second (first L)) (first (second L)) (second (second L))))

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

(define (invertPlayer human)
  (cond
    ((equal? human #T) #F)
    (else #T)))

(define (randomIndexFrom L)
  (list-ref L (random (length L))))
  
