#lang racket
(require racket/list)

(define-struct state (board score color parent #|kingAttacked?|#))


(define B1 '(("WR" "WH" "WB" "WQ" "WK" "WB" "WH" "WR")
             ("WP" "WP" "WP" "WP" "WP" "WP" "WP" "WP")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("--" "--" "--" "--" "--" "--" "--" "--")
             ("BP" "BP" "BP" "BP" "BP" "BP" "BP" "BP")
             ("BR" "BH" "BB" "BQ" "BK" "BB" "BH" "BR")))

(define TST '(("WQ" "WK" "--") ;black king can kill the rook, lets just say its not the best idea
              ("--" "--" "--")
              ("--" "BK" "BQ")))


(define b #\B) ;just for ease of input
(define w #\W) ;yeah, im THAT lazy

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


;movement

;pawn section (its the only piece that gets one...)
(define (PawnPossibleBoards B Xpos Ypos [color (getColor B Xpos Ypos)]) ;returns boards
  (let ([side  (sideFinder color)])
    (cond
      ((isOnStartingLane? Ypos (invertColor color)) (crownChecker B Xpos Ypos color (removeAllOccurrencesOf '() (list (pawnMoves-regularKills B Xpos Ypos side) (pawnMoves-regualarMove B Xpos Ypos side)))))
      (else
       (filterMovelessPieces B
                         (cons (list Xpos Ypos)
                               (flatten-lists
                                (removeAllOccurrencesOf '()
                                                        (list (pawnMoves-regularKills B Xpos Ypos side)
                                                              (pawnMoves-startingLane B Xpos Ypos side color)  
                                                              '()))))))))) ;more moves soon (ummmm NOPE XD)
;need to add crowning, the only reason the game (yes... the one in which the kings are dead 10 turnds in...)
;crashed is cuse a pawn gets to the last lane and tries to move the next turn, getting to index 8 (out of 7) and crashing

(define (PawnPossibleMoves B Xpos Ypos [player #F] [color (getColor B Xpos Ypos)]) ;returns moves, and asks for input about crowning 'result'
  (let ([side  (sideFinder color)])     ;player will trigger the crown choise, else it's gonna just predict the move without crowning
    (cond
      ((and player (isOnStartingLane? Ypos (invertColor color))) (choose-crowned B Xpos Ypos color (removeAllOccurrencesOf '() (list (pawnMoves-regularKills B Xpos Ypos side) (pawnMoves-regualarMove B Xpos Ypos side)))))
      (else
       (cons (list Xpos Ypos)
             (flatten-lists
              (removeAllOccurrencesOf '()
                                      (list (pawnMoves-regularKills B Xpos Ypos side)
                                            (pawnMoves-startingLane B Xpos Ypos side color)  
                                            '()))))))))

(define (choose-crowned B Xpos Ypos color movesL) ;origin X and Y, color... well, and the possible moves as input
  (displayln "pick a move (index)")                ;it's a combination of 'selectMove' and 'Pawn-Crowning'
  (print (rest movesL))
  (let ([selectedMove (add1 (read))])
    (cond
      ((= selectedMove (length movesL)) (displayln "out of list input") (newline) (choose-crowned B Xpos Ypos color movesL))
      (else
       (let ([selectedX (first (list-ref movesL selectedMove))]
             [selectedY (second (list-ref movesL selectedMove))])
         (updateBoard (clearTileAt B Xpos Ypos) selectedX selectedY (choosePiece color)))))))

(define (choosePiece color)
  (displayln "pick crowned piece:")
  (displayln "Queen (Q) 1")
  (displayln "Rook (R) 2")
  (displayln "Knight (H) 3")
  (displayln "Bishop (B) 4")
  (let ([piece (read)])
    (cond
      ((= piece 1) (makePiece #\Q color))
      ((= piece 1) (makePiece #\R color))
      ((= piece 1) (makePiece #\H color))
      ((= piece 1) (makePiece #\B color))
      (else (displayln "please pick a valid number") (newline)  (choosePiece color)))))

(define (pawnMoves-regualarMove B Xpos Ypos side)
  (cond
    ((equal? (getTileAt B Xpos (+ Ypos side)) "--") (list Xpos (+ Ypos side))) ;WARNING! returns a 'single layered' list (so be carefull... it MAY fuckup someting... somehow)
    (else '()))) ;defult 'no new move here' output that will get filtered out

(define (pawnMoves-startingLane B Xpos Ypos side [color (getColor B Xpos Ypos)]) 
  (cond                                    ;the 'side' inverts the movement of the pawn (its the color...)
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

(define (Pawn-Crowning B Xpos Ypos color L) ;I'll add the pawn boards on their own cuse the crowning isnt a 'regular' move
  (let ([originX Xpos]
        [originY Ypos]
        [ratgetX (first (first L))]
        [targetY (second (first L))])
    (cond
      ((empty? (rest L)) (list (moveAndTurnInto B originX originY ratgetX targetY #\Q color)
                               (moveAndTurnInto B originX originY ratgetX targetY #\R color)
                               (moveAndTurnInto B originX originY ratgetX targetY #\B color)
                               (moveAndTurnInto B originX originY ratgetX targetY #\H color)))
          
      (else (append (list (moveAndTurnInto B originX originY ratgetX targetY #\Q color)
                          (moveAndTurnInto B originX originY ratgetX targetY #\R color)
                          (moveAndTurnInto B originX originY ratgetX targetY #\B color)
                          (moveAndTurnInto B originX originY ratgetX targetY #\H color)) (Pawn-Crowning B Xpos Ypos color (rest L)))))))

(define (crownChecker B Xpos Ypos color L) ;filteres out 'failed crownings' (pawn can't crown and the function cant handle that XD)
  (cond
    ((empty? L) '())
    (else (Pawn-Crowning B Xpos Ypos color L))))

(define (moveAndTurnInto B originX originY ratgetX targetY type color)
  (clearTileAt (updateBoard B ratgetX targetY (makePiece type color)) originX originY))
      

(define (sideFinder color) ;again, its simple... (returns the DEFULT change in Y)
  (cond
    ((equal? color #\W) 1)
    (else -1)))

(define (pawnMoves-regularKills B Xpos Ypos side [L '(1 -1)] [color (getColor B Xpos Ypos)])
    (cond
      ((empty? L) '())
      ((and (legalTile? B (+ Xpos (first L)) (+ Ypos side)) (kill? B (+ Xpos (first L)) (+ Ypos side) color)) (append (list (+ Xpos (first L)) (+ Ypos side)) (pawnMoves-regularKills B Xpos Ypos side (rest L) color)))
      (else (pawnMoves-regularKills B Xpos Ypos side (rest L) color))))

;regular move - 1 tile                                    DONE
;kills 1 diagonal left or rigth from movement direction   DONE
;first move - 2 tiles                                     DONE
;capture during first move                                      some hard shit here, i need to save the last move
;croned at the end of the board                           DONE


;Knight movement ;WORKING
(define (KnightPossibleMoves B Xpos Ypos [color (getColor B Xpos Ypos)])
  (cons (list Xpos Ypos) (Knight-addPossibleMovesFromList B Xpos Ypos (list '(2 -1) '(-2 -1)
                                                                            '(2 1)  '(-2 1)
                                                                            '(1 -2) '(-1 -2)
                                                                            '(1 2)  '(-1 2)) color)))
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

;casteling       gonna be a b**** (and idk if i'll bother)


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
(define (KingPossibleMoves B Xpos Ypos [color (getColor B Xpos Ypos)])
  (cons (list Xpos Ypos)
        (King-addPossibleMovesFromList B Xpos Ypos color))) ;to get the king out of the way of potential attackers
                                        

(define (King-addPossibleMovesFromList B originX originY color [L (list '(1 -1) '(1 0) '(1 1) '(0 1) '(0 -1) '(-1 -1) '(-1 0) '(-1 1))])
  (cond 
      ((empty? L) '())
      ((not (LegalMove? B (+ (first (first L)) originX) (+ (second (first L)) originY) color)) (King-addPossibleMovesFromList B originX originY color (rest L)))
      (else (cons (list (+ (first (first L)) originX) (+ (second (first L)) originY)) (King-addPossibleMovesFromList B originX originY color (rest L))))))


;move options
(define (lookLine B Xpos Ypos color [Xchange 0] [Ychange 1] [ignoreTile #T]) ;ONLY one of the cnages must be active
  (cond                                                                ;defult is WHITE pawn movement (-1 for black pawn)
    (ignoreTile (lookLine B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F)) ;to ingore the origin location
    ((kill? B Xpos Ypos color) (cons (list Xpos Ypos) '()))
    ((not (LegalMove? B Xpos Ypos color)) '())
    (else (cons (list Xpos Ypos) (lookLine B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F)))))

(define (lookDiagonal B Xpos Ypos color [Xchange 1] [Ychange 1] [ignoreTile #T]) ;the cnages are 1 or -1
  (cond                                                                    ;defult is bottom right diagonal
    (ignoreTile (lookDiagonal B (+ Xpos Xchange) (+ Ypos Ychange) color Xchange Ychange #F))
    ((kill? B Xpos Ypos color) (cons (list Xpos Ypos) '())) ;to prevent further checks if the piece finds a kill (it cant over or kill multiple pieses in one turn)
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

(define (kill? B Xtarget Ytarget attackerColor) ;legal tiles assumed, its an inner function (NOPE.... not anymore XD)
  (cond
    ((or (not (legalTile? B Xtarget Ytarget)) (equal? (getTileAt B Xtarget Ytarget) "--") (equal? (getColor B Xtarget Ytarget) attackerColor)) #F)
    (else #T)))

(define (threatenedTile? B Xpos Ypos [attackedColor (getColor B Xpos Ypos)]) ;needs a color input (black or white) if not given X and Y of a piece
  (let ([dummy (makePiece #\D attackedColor)]
        [ATKcolor (invertColor attackedColor)])
    (let ([newB (updateBoard B Xpos Ypos dummy)])
      (checkDummyOnAllBoards (removeAllOccurrencesOf '() (allNewBoards newB ATKcolor))))))


;printing
(define (printBoard B) ;prints the board
  (for-each displayln B))

(define (printAllBoards L) ;prints all listed boards
  (cond
    ((empty? (rest L)) (printBoard (first L)))
    (else (printBoard (first L)) (newline) (printAllBoards (rest L)))))

(define (printState state)
  (printBoard (state-board state))
  (println (state-score state))
  (println (state-color state))) ;printint the parent is kinda pointless

(define (printAllStates states)
  (cond
    ((empty? states) 'done)
    (else (printState (first states)) (printAllStates (rest states)))))

(define (printAllGroups groupedStates)
  (for-each (lambda (states) (displayln "NEXT PARENT GROUP") (printAllStates states) (newline)) groupedStates))

(define (printAllGameHistory state)
  (cond
    ((equal? (state-parent state) 'none) 'done)
    (else (let ([B (state-board state)]
                [parent (state-parent state)])
            (printBoard B) (newline)
            (printAllGameHistory parent)))))

(define (printAllScoreGroups groups)
  (cond
    ((empty? groups) 'done)
    (else (display "the ") (print (state-score (first (first groups)))) (displayln " group:") (printAllStates (first groups)) (newline) (printAllScoreGroups (rest groups)))))


;gameTypes
(define (PVP B [color #\W])
  (cond
    ((win? B color) (print (invertColor color)) (display " ") (displayln "won") (newline))
    ((empty? (filterChecked B color)) (displayln "stalemate") (newline))
    (else
     (cond
       ((equal? color #\W) (displayln "white's turn"))
       (else (displayln "black's turn")))
     (PVP (selectTile B color) (invertColor color)))))

(define (selectTile B [color #\W]) ;will be graphical later, but still use the same core so it's ok
  (printBoard B)
  (displayln "enter X of tile (up to 7)")
  (define Xpos (read))
  (displayln "enter Y of tile (up tp 7)")
  (define Ypos (read))
  (moveOptions B Xpos Ypos color))

(define (PVEdemo [B B1] [color #\W] [human #T]) ;its a completly random bot
  (cond
    ((win? B color) (print color) (displayln " won"))
    (else
     (cond
       ((equal? color #\W) (displayln "white's turn"))
       (else (displayln "black's turn")))
     (cond
       (human (PVEdemo (selectTile B color) (invertColor color) (invertPlayer human)))
       (else (let ([newB (state-board (lazyMinMax 2 (SB B color)))])
                     (PVEdemo newB (invertColor color) (invertPlayer human))))))))

(define (EVEbullshit [B B1] [color #\W] [turnCounter 1] [turnsToTie 50] [lastPieceCount (+ (length (findAllColor B w)) (length (findAllColor B b)))]) ;its a completly random bot duel to the crash!
  (printBoard B)
  (cond
    ((= turnsToTie 0) (displayln "stalemate") (newline))
    ((win? B color) (print (invertColor color)) (display " ") (displayln "won") (newline))
    ((empty? (filterChecked B color)) (displayln "stalemate") (newline))
    (else                          ;its sooo bad
     (display "turn ") (println turnCounter)
     (cond
       ((equal? color #\W) (displayln "white's turn"))
       (else (displayln "black's turn")))
     (newline)
     (let ([newB (state-board (lazyMinMax 2 (SB B color)))]
           [pieceCount (+ (length (findAllColor B w)) (length (findAllColor B b)))])
       (cond
         ((= lastPieceCount pieceCount) (EVEbullshit newB (invertColor color) (add1 turnCounter) (sub1 turnsToTie) pieceCount))
         (else (EVEbullshit newB (invertColor color) (add1 turnCounter) 50 pieceCount)))))))
   

;board operations
(define (findKing B color)
  (searchForKing B (makePiece #\K color)))

(define (searchForKing B [target (makePiece #\K #\W)] [Xpos 0] [Ypos 0]) ;the same, but stops after finding one king (i can change the original buttttt later)
  (cond
    ((= Ypos (length B)) '()) ;can cause problems... but maybe not... i'm filtereint these out on every function (and the random game was fine...)
    ((= Xpos (length (first B))) (searchForKing B target 0 (add1 Ypos)))
    ((equal? (getTileAt B Xpos Ypos) target) (list Xpos Ypos))
    (else (searchForKing B target (add1 Xpos) Ypos))))


(define (findAllColor B color [Xpos 0] [Ypos 0])
  (cond
    ((= Ypos (length B)) '())
    ((= Xpos (length (first B))) (findAllColor B color 0 (add1 Ypos)))
    ((equal? (getColor B Xpos Ypos) color) (cons (list Xpos Ypos) (findAllColor B color (add1 Xpos) Ypos)))
    (else (findAllColor B color (add1 Xpos) Ypos))))

(define (findAllType B type [Xpos 0] [Ypos 0]) ;its soooooooo similar to findAllColor, only 1 word change (easy fix, but not now)
  (cond
    ((= Ypos (length B)) '())
    ((= Xpos (length (first B))) (findAllType B type 0 (add1 Ypos)))
    ((equal? (getType B Xpos Ypos) type) (cons (list Xpos Ypos) (findAllType B type (add1 Xpos) Ypos)))
    (else (findAllType B type (add1 Xpos) Ypos))))

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
    (else (print 'invalid-tile))))

(define (getTileAt B Xpos Ypos) ;returnes a tile in a given location
  (list-ref (list-ref B Ypos) Xpos))



;board-wide move managment
(define (allMovesForColor B color [L (findAllColor B color)]);RETURNS a list of all origin points of pieses and tiles they van move into 
  (cond                                                      ;or just the origin in one move is avalible
    ((empty? (rest L)) (list (possibleMovesForTile B (first (first L)) (second (first L)))))
    (else (cons (possibleMovesForTile B (first (first L)) (second (first L)))  (allMovesForColor B color (rest L))))))

(define (addOriginPosToDestanation L [index 1]) ;index 0 is the origin ;) (no bugs XD)
  (cond
    ((= index (length L)) '())
    (else (cons (list (first L) (list-ref L index)) (addOriginPosToDestanation L (add1 index))))))


(define (filterChecked B color [L (allNewBoards B color)])
    (ignoreBadMoves L color)) ;just an activator for a better function that ACTUALLY does its job

(define (ignoreBadMoves L color) ;'bad moves' are when the king is cheched and left that way (no idea for a better name... (psssst, what about 'can't run or be defended'?)
  (cond                ;L is a LIST of BOARDS
    ((empty? L) '()) ;defult return
    ((attackedKing? (first L) color) (ignoreBadMoves (rest L) color)) ;good move passed on
    (else (cons (first L) (ignoreBadMoves (rest L) color))))) ;bad move removed from list


(define (makeMove B L) ;GETS a single move '((originX originY) (destX destY))
                       ;RETURNS a board updated after the given move
  (moveTo B (first (first L)) (second (first L)) (first (second L)) (second (second L))))

;special conditions (wins, draws and other stuff) 
(define (win? B color [start #F]) ;from the prespective of the WINNER
  (let ([enemyColor (invertColor color)])
    (cond ;start is true of falce (by defult) based on the mite of the ceck, before or after the side's move
      ((empty? (findKing B enemyColor)) #T) ; the king is already dead
      ((and start (attackedKing? B enemyColor)) #T) ; the king is under attack in the beggining of the turn (so he'll BE killed)
      (else #F))))

;abit tooooo agressive about putting games down
(define (draw? B color) ;I may have missed someting, and every option is a new line ro make it easier to read and understand (so its not a page-long or)
  (cond
    ((and (not (attackedKing? B color)) (empty? (filterChecked B color))) #T) ;no moves, king NOT under attack
    ((and (= (length (findAllColor B #\W)) 1) (= (length (findAllColor B #\B)) 1)) #T) ;only kings left (i need to disallow killing the king for that to work properly (done))
    ((and (= (+ (length (findAllColor B #\B)) (length (findAllColor B #\W))) 3) (or (findAllType B #\B)       ;king + bishop VS king (there is a similar thing with multiple bishpps on the same color)
                                                                                    (findAllType B #\H))) #T) ;king + knight VS king
    (else #F))) ;i think it's everyting... ignoring (just for now) the multiplu same colored bishops
;and the 3 repetitions of the same state - i need this for bots, they can get stuck in a 'checkNblock' sycle
;and the 50 moves without kills, but i belive it wont happen... plus i've banned most of the endgame causes (but a rook or a queen can play badly and fail to win in 50 turns)


;general use
(define (attackedKing? B color)
  (let ([kingPos (findKing B color)])
    (cond
      ((threatenedTile? B (first kingPos) (second kingPos)) #T)
      (else #F))))
     
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
    ((equal? color #\B) #\W)
    (else 'ERR-color)))

(define (invertPlayer human)
  (cond
    ((equal? human #T) #F)
    (else #T)))

(define (randomIndexFrom L)
  (list-ref L (random (length L))))

(define (makePiece type color)
  (string color type))

(define (isInRange? num start end)
  (cond
    ((and (> num (sub1 start)) (< num (add1 end))) #T)
    (else #F)))

(define (round* num) ;just to get rid of the 0.99999999999999999 (you know what i'm talking about...)
  (/ (floor (* 100 num)) 100))

(define (insertToEnd item list)
  (reverse (cons item (reverse list))))


;reworked (ummmmmmm just the giant pile of new code I added)
(define (boardsOfAllMoves B L [originX (first (first L))] [originY (second (first L))]) ;L is an output of the possibleMoves functions, of format ((originX originY) (targetX targetY)....)
  (let ([targetX (first (second L))]                                                    ;RETURNS the final BOARD of every move
        [targetY (second (second L))]
        [move (list (first L) (second L))])
    (cond
      ((empty? (drop L 2)) (list (makeMove B move)))
      (else (cons (makeMove B move) (boardsOfAllMoves B (cons (first L) (drop L 2)) originX originY))))))


(define (allNewBoards B color [pieces (findAllColor B color)]) ;pieces is the locations, given by find all
                                ;NEEDS A '() cleanup on every call just to make sute it'll work properly
#| debug
  (println pieces)
  (printBoard B)
  (println (getTileAt B (first (first pieces)) (second (first pieces))))
  (newline) |#
  
  (let ([pieceX (first (first pieces))]
        [pieceY (second (first pieces))])
    (cond
      ((empty? (rest pieces)) (possibleBoardsForTile B pieceX pieceY))
      (else (append (possibleBoardsForTile B pieceX pieceY) (allNewBoards B color (rest pieces)))))))


(define (possibleBoardsForTile B Xpos Ypos [target (getType B Xpos Ypos)]) ;only the bot uses this functuion

#| debug
  (printBoard B)
  (newline) |#
  
  (cond
    ((equal? target #\P) (PawnPossibleBoards B Xpos Ypos)) ;pawns can crown, the regular move system can't create new pieces so they have a different board creating system
    ((equal? target #\B) (filterMovelessPieces B (BishopPossibleMoves B Xpos Ypos)))
    ((equal? target #\H) (filterMovelessPieces B (KnightPossibleMoves B Xpos Ypos)))
    ((equal? target #\R) (filterMovelessPieces B (RookPossibleMoves B Xpos Ypos)))
    ((equal? target #\Q) (filterMovelessPieces B (QueenPossibleMoves B Xpos Ypos)))
    ((equal? target #\K) (filterMovelessPieces B (KingPossibleMoves B Xpos Ypos)))
    (else '())))

(define (filterMovelessPieces B L) ;just a buffer between 'boardsOfAllMoves' and deadly input (but no JK, it stops it from crushing...)
  (cond
    ((empty? (rest L)) '())
    (else (boardsOfAllMoves B L))))

(define (possibleMovesForTile B Xpos Ypos [target (getType B Xpos Ypos)]) ;this is for the player
  (cond
    ((equal? target #\P) (PawnPossibleMoves B Xpos Ypos #T))
    ((equal? target #\B) (BishopPossibleMoves B Xpos Ypos))
    ((equal? target #\H) (KnightPossibleMoves B Xpos Ypos))
    ((equal? target #\R) (RookPossibleMoves B Xpos Ypos))
    ((equal? target #\Q) (QueenPossibleMoves B Xpos Ypos))
    ((equal? target #\K) (KingPossibleMoves B Xpos Ypos))
    (else '())))


#| will be adrresed in the scoring sunction |# 
(define (freeStyleKingBoards B color [kingX (first (findKing B color))] [kingY (second (findKing B color))]) ;will return all the boards from kingFreeStyleMoves
  (boardsOfAllMoves B (cons (list kingX kingY) (freeStyleKingMoves B color kingX kingY))))

(define (freeStyleKingMoves B color kingX kingY [L (list '(1 -1) '(1 0) '(1 1) '(0 1) '(0 -1) '(-1 -1) '(-1 0) '(-1 1) "don't delete me")])
  (let ([newX (+ kingX (first (first L)))]
        [newY (+ kingY (second (first L)))])
    (cond
      ((empty? (rest L)) '())
      ((and (legalTile? B newX newY) (not (friendlyTile? B newX newY color))) (cons (list newX newY) (freeStyleKingMoves B color kingX kingY (rest L))))
      (else (freeStyleKingMoves B color kingX kingY (rest L))))))
#| |#


(define (checkDummyOnAllBoards L [ATKcounter 0]) ;L is all the enemy's moves
  (cond
    ((empty? L) (outputAnalayzer ATKcounter))
    ((deadDummy? (first L)) (checkDummyOnAllBoards (rest L) (add1 ATKcounter)))
    (else (checkDummyOnAllBoards (rest L) ATKcounter))))

(define (outputAnalayzer ATKcounter)
  (cond
    ((= 0 ATKcounter) #F)
    (else ATKcounter)))

(define (deadDummy? B)
  (empty? (findAllType B #\D)))


(define (moveOptions B Xpos Ypos color) ;color is the current player (this function gets called by 'selectTile' in the 'main' section)
  (cond
    ((not (equal? (getColor B Xpos Ypos) color)) (displayln "pick your own piece") (newline) (selectTile B color))
    (else (selectMove B (possibleMovesForTile B Xpos Ypos) color))))


(define (selectMove B movesL color) ;movesL is a LIST of MOVES (hummmm i'll need a spesial function for the pawn... later)
  (cond                             ;WILL NOT WORK on pawns about to crown, yet
    ((empty? (rest movesL)) (display "can't move, ") (reasonGiver B color) (newline) (selectTile B color))
    (else
     (let ([originX (first (first movesL))]
           [originY (second (first movesL))])
       (displayln "pick a move (index):")
       (displayln (rest movesL)) ;to ignore origin location
       (newline) 
       (let ([moveIndex (add1 (read))]) ;to skip origin location (minimum index is 1)
         (moveTo B originX originY (first (list-ref movesL moveIndex)) (second (list-ref movesL moveIndex))))))))

(define (reasonGiver B color) ;just some more info, will later be replaced with text displayed in the game window
  (cond
    ((attackedKing? B color) (displayln "the king is under attack"))
    (else (displayln "all moves are blocked"))))

(define (test [B B1] [times 10] [counter 0])
  (cond
    ((= counter times) (newline) (display "no crash"))
    (else (display "game No ") (println (add1 counter)) (EVEbullshit B) (test B times (add1 counter)))))


;scoring
;value of each piece: (set in stone)
;Pawn - 1
;Bishop - 3
;Knight - 3
;Rook - 5
;Queen - 9
;King - game (JK... but realy, its game over if you lose him so its infinity)

;bounuses are apresent of the piece value (so a pawn wont be worth more that a rook)
(define mountaintopBounus 1.25) 
(define hillsBounus 1.1)
(define vallyBounus 1)
(define swampBounus 0.9)
;maybe i'll add 'controlled area' that will count the total tiles a color can move to (including attacks... so basicly all possible moves)
;               'enamy cheched' a set bounus on attaking the king


(define (scoreForBoard B color [start #F]) ;returns a score for the given board, both colors return the sane score (only difference being -/+inf.0 from the win condition)
  (let ([winResult (winCheck B color start)])
    (cond
      (winResult winResult)
      (else (round* (- (calcScore B #\W (findAllColor B #\W)) (calcScore B #\B (findAllColor B #\B))))))))
  
(define (winCheck B color [start #F]) ;rerurns a score of -inf.0, +ilf.0 or #F if no win
  (cond
    ((win? B color start) (winValue color))
    (else #F)))

(define (winValue color) ;returns the 'target' (AKA win) value for the given color
  (cond
    ((equal? color #\W) +inf.0)
    (else -inf.0)))

 
(define (calcScore B color [pieces (findAllColor B color)])
  (let ([pieceX (first (first pieces))]
        [pieceY (second (first pieces))])
  (cond
    ((empty? (rest pieces)) (giveValueToPiece B pieceX pieceY))
    (else (+ #| new scoring goes here |# (giveValueToPiece B pieceX pieceY) (calcScore B color (rest pieces)))))))

(define (giveValueToPiece B pieceX pieceY)
  (let ([type (getType B pieceX pieceY)])
    (cond
      ((equal? type #\K) 0) ;ummm ikd waht to do... so... yea
      ((isInHilltop? (list pieceX pieceY)) (* (baseValue type) mountaintopBounus))
      ((isInHills? pieceX pieceY) (* (baseValue type) hillsBounus))
      ((isInVally? pieceX pieceY) (* (baseValue type) vallyBounus))
      (else (* (baseValue type) swampBounus))))) ;ummm everyting else is in the swamp

(define (baseValue type)
  (cond
    ((equal? type #\P) 1)
    ((equal? type #\B) 3)
    ((equal? type #\H) 3)
    ((equal? type #\R) 5)
    ((equal? type #\Q) 9)
    (else 0))) ;king will go here (theoreticly... that function is never run on him)

(define (isInHilltop? XYpos [L (list '(3 3) '(3 4) '(4 3) '(4 4))])
  (cond
    ((empty? L) #F)
    ((equal? XYpos (first L)) #T)
    (else (isInHilltop? XYpos (rest L)))))

(define (isInHills? X Y)
  (cond
    ((and (= X 2) (isInRange? Y 2 5)) #T) 
    ((and (= X 5) (isInRange? Y 2 5)) #T)
    ((and (= Y 2) (isInRange? X 2 5)) #T) 
    ((and (= Y 5) (isInRange? X 2 5)) #T)
    (else #F)))

(define (isInVally? X Y)
  (cond
    ((and (= X 1) (isInRange? Y 1 6)) #T) 
    ((and (= X 6) (isInRange? Y 1 6)) #T)
    ((and (= Y 1) (isInRange? X 1 6)) #T) 
    ((and (= Y 6) (isInRange? X 1 6)) #T)
    (else #F)))


;state makers
(define (allMovesToStates parent [color (state-color parent)] [L (allNewBoards (state-board parent) color)]) ;L is all possible BOARDS
  (cond
    ((empty? L) '())
    (else (cons (make-state (first L) 0 (invertColor color) parent) (allMovesToStates parent color (rest L))))))
                                     ;the score is calculated only at max depth

       ;SB => State (from) Board (im lazy ;))
(define (SB B [color w] [parent 'none]) ;just converts aboard to a state
  (make-state B (scoreForBoard B color #T) color parent))

(define (calcScoreForList L) ;L is a list of states
    (cond
      ((empty? L) '())
      (else   (let ([B (state-board (first L))]
                    [color (state-color (first L))] 
                    [parent (state-parent (first L))])
                (cons (make-state B (scoreForBoard B color) color parent) (calcScoreForList (rest L)))))))
 ;the color is the next move so you need to invert it to evaluate the move just made


;debug tool(s)
(define (listToBoard LL) ;LL = List List ;)
  (cond
    ((empty? LL) '())
    (else (cons (lineFormat (first LL)) (listToBoard (rest LL))))))

(define (lineFormat L)
  (cond
    ((empty? L) '())
    (else (cons (string (first L))  (lineFormat (rest L))))))
                ;this dosent work (and its only debug so IDC)

(define (cheakKing B color)
  (let ([kingPos (findKing B color)])
    (KingPossibleMoves B (first kingPos) (second kingPos))))


;minimax ;need optimazing so it wont save all the states at once
(define (lazyMinMax depth [state start] [L (allStatesToDepth depth state)])
  (println (length L))
  (cond
    ((empty? (rest L)) (min\max (first L))) ;it means there's only one group so just minimax the rest
    (else (lazyMinMax depth state (group-by (lambda (state) (state-parent state)) (map (lambda (group) (updateParent group)) L))))))

#| abandoned attempt
(define (prossesGroup depth state) ;WIP - will prosses one branch at a time, not all atonvce with a map function
  (cond
    ((= depth 1) (min\max (calcScoreForList (allMovesToStates state)))) ;returns the same state but with the best child's score
    (else 'WIP)))
|#

(define (runTST depth [state start])
  (cond
    ((= depth 0) state)
    (else (tstfunction depth (allMovesToStates state)))))
  

(define (tstfunction depth open [state (first open)]) ;open is (allMovesToStates state)
  ;(displayln "all oppenent moves from: ") (printState (state-parent state)) (newline) (printAllStates open) (newline)
  (println depth)
  (cond
    ((= depth 1) (list (min\max (calcScoreForList open))))
    ((empty? (rest open)) (list (tstfunction (sub1 depth) (allMovesToStates state)))) ;the first line is more likly so... preformance boost
    (else (updateParent (cons (tstfunction (sub1 depth) (allMovesToStates state))
                              (tstfunction depth (rest open))))))) ;not working and I have no idea y


(define (updateParent childrenGroup) ;will return a state with the parent's board but the score of the best child
  ;(display "STATES: ") (println childrenGroup)
  (let ([parent (traceBack (first childrenGroup) 1)])
    ;(printState parent)
    (let ([parentBoard (state-board parent)]
          [parentColor (state-color parent)]
          [miniMaxedScore (state-score (min\max childrenGroup))]
          [grandParent (traceBack parent 1)])
      ;(printState (make-state parentBoard miniMaxedScore parentColor grandParent))
      ;(newline)
      (make-state parentBoard miniMaxedScore parentColor grandParent))))

;needs to be outed
(define (allStatesToDepth depth [state start])
  (map (lambda (states) (calcScoreForList states)) (group-by (lambda (state) (state-parent state)) (developAllMoves depth (allMovesToStates state))))) ;returns groups of ALL final moves up to the given depth

;needs to be outed
(define (developAllMoves depth [L (allMovesToStates state)])
  (cond
    ((= depth 0) L)
    (else (developAllMoves (sub1 depth) (flatten (map (lambda (state) (allMovesToStates state)) L))))))


(define (copyAndGiveScore state) ;copies over all the state's detailes and adds a score
  (let ([B (state-board state)]
        [color (state-color state)]
        [parent (state-parent state)])
    (make-state B (scoreForBoard B color #T) color parent)))

(define (traceBack state depth) ;returns 'depth' genarations of parents back
  (cond
    ((= depth 0) state)
    (else (traceBack (state-parent state) (sub1 depth)))))


(define (min\max states) ;just sorting into min or max by the color
  (let ([color (state-color (state-parent (first states)))])
    (cond
      ((equal? color #\W) (randomIndexFrom (first (sort* states))))
      (else (randomIndexFrom (last (sort* states)))))))

(define (sort* states) ;used to sort the groups of states by score from high to low
  (sort (group-by (lambda (state) (state-score state)) states) (lambda (a b) (> (state-score (first a)) (state-score (first b))))))


;startup
(define start (make-state B1 0 #\W 'none))

;random shit
(define (crazyMyltiplay L1 L2)
  (map (lambda (L) (map (lambda (x) (* L x)) L1)) L2))


(define (CT [L '(1 2 3 4 5 6 7 8 9 10)]) ;CT - Consept Test
  (cond
    ((empty? (rest L)) (list (list (first L) (* 2 (first L)))))
    (else (cons (list (first L) (* 2 (first L)))
                (CT (rest L))))))


(define (randomShit)
(cons (tstfunction 1 (allMovesToStates start))
 (cons (tstfunction 1 (drop (allMovesToStates start) 1))
  (cons (tstfunction 1 (drop (allMovesToStates start) 2))
   (cons (tstfunction 1 (drop (allMovesToStates start) 3))
    (cons (tstfunction 1 (drop (allMovesToStates start) 4))
     (cons (tstfunction 1 (drop (allMovesToStates start) 5))
      (cons (tstfunction 1 (drop (allMovesToStates start) 6))
       (cons (tstfunction 1 (drop (allMovesToStates start) 7))
        (cons (tstfunction 1 (drop (allMovesToStates start) 8))
         (cons (tstfunction 1 (drop (allMovesToStates start) 9))
          (cons (tstfunction 1 (drop (allMovesToStates start) 10))
           (cons (tstfunction 1 (drop (allMovesToStates start) 11))
            (cons (tstfunction 1 (drop (allMovesToStates start) 12))
             (cons (tstfunction 1 (drop (allMovesToStates start) 13))
              (cons (tstfunction 1 (drop (allMovesToStates start) 14))
               (cons (tstfunction 1 (drop (allMovesToStates start) 15))
                (cons (tstfunction 1 (drop (allMovesToStates start) 16))
                 (cons (tstfunction 1 (drop (allMovesToStates start) 17))
                  (cons (tstfunction 1 (drop (allMovesToStates start) 18))
                   (list (tstfunction 1 (drop (allMovesToStates start) 19))
                   )))))))))))))))))))))





         
         
         
