(define (attackedTile? B Xpos Ypos [attackedColor (getColor B Xpos Ypos)]) ;there IS A LOT of optimization to be done here... all the functions look mostly the same
  (define dummy (makePiece #\D attackedColor))                                 ;I'll worry about refactoring later, its modular anyway

  (let ([newB (updateBoard B Xpos Ypos dummy)]) ;placing a target so nearby pieces will see it as a legal move
    
    #| debug 
    (printBoard B)
    (newline)
    (println dummy)
    (printBoard newB)
    (display "Knight: ")        (println (attackedByKnight newB Xpos Ypos))
    (display "BishopOrQueen: ") (println (attackedByBishopOrQueen newB Xpos Ypos))
    (display "RookOrQueen: ")   (println (attackedByRookOrQueen newB Xpos Ypos))
    (display "Pawn: ")          (println (attackedByPawn newB Xpos Ypos))
    (display "King: ")          (println (attackedByKing newB Xpos Ypos attackedColor))
    (newline)
    |#
    
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
    ((empty? L) ATKCounter) ;it wont get here if ATKCounter is at 0
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

(define (attackedByPawn B Xpos Ypos [targetColor (getColor B Xpos Ypos)] [ATKCounter 0] [L (list '(1 1) '(-1 1) '(1 -1) '(-1 -1) '(10 10 "dont delete me"))])
  (let ([newX (+ Xpos (first (first L)))]                                                                                        ;to avoid skipping the last attack check
        [newY (+ Ypos (second (first L)))])
    (cond
      ((and (empty? (rest L)) (zero? ATKCounter)) #F) ;i need valus to define the let so i have to be sure i have someting in the list
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
    
    
(define (possibleMovesForTile B Xpos Ypos [target (getType B Xpos Ypos)])
  (cond
    ((equal? target #\P) (PawnPossibleMoves B Xpos Ypos))
    ((equal? target #\B) (BishopPossibleMoves B Xpos Ypos))
    ((equal? target #\H) (KnightPossibleMoves B Xpos Ypos))
    ((equal? target #\R) (RookPossibleMoves B Xpos Ypos))
    ((equal? target #\Q) (QueenPossibleMoves B Xpos Ypos))
    ((equal? target #\K) (KingPossibleMoves B Xpos Ypos))
    (else 'ERR-cant-recognize-piece)))
    
    
    (define (finalLaneFinder color)
  (cond
    ((equal? color #\W) 7)
    (else 0)))

(define (isOnLastLane? Ypos color)
  (cond
    ((= Ypos (finalLaneFinder color)) #T)
    (else #F)))


(define (pawn-Crowning B Xpos Ypos)
  (list (list (Xpos Ypos) #\H)
        (list (Xpos Ypos) #\B)
        (list (Xpos Ypos) #\R)
        (list (Xpos Ypos) #\Q)))


(define (moveOptions B Xpos Ypos color)
  (cond
    ((not (equal? (getColor B Xpos Ypos) color)) (displayln "pick your own piece") (newline) (selectTile B color))
    (else (selectMove B (possibleMovesForTile B Xpos Ypos) color))))

(define (selectMove B movesL color) ;its can force a move that leaves the king attacked, the search per piece needs to change
  (cond
    ((empty? (rest movesL)) (displayln "can't move") (selectTile B color))
    (else 
     (displayln "pick a move (index):")
     (displayln (rest movesL)) ;to ignore origin location
     (newline) 
     (define moveIndex (add1 (read)))
     (moveTo B (first (first movesL)) (second (first movesL)) (first (list-ref movesL moveIndex)) (second (list-ref movesL moveIndex))))))

    
#| replaced by 'allNewBoards'
(define (getAllMovesForColor B color) ;just removes (removes what??? (JK) it removes all 'empty moves' (as in piesec that have mo moves) from the move list)
  (removeAllOccurrencesOf '() (allPossibleMovesForColor B color)))


(define (makeAllMoves B color [L (allPossibleMovesForColor B color)]) ;first cuse its a long list
  (cond
    ((= 3 (length L)) (makeLastMove B color L))
    (else 
     (let ([originX (first (first (first L)))]
           [originY (second (first (first L)))]
           [targetX (first (second (first L)))]
           [targetY (second (second (first L)))])
       (cond
         ((empty? (rest L)) '())
         (else (cons (moveTo B originX originY targetX targetY) (makeAllMoves B color (rest L))))))))) ;YASSSSSSSSSSS working (WOW there... calm down... kids these days... (XD))

(define (makeLastMove B color L)
  (let ([originX (first (first L))]
        [originY (second (first L))]
        [targetX (first (second L))]
        [targetY (second (second L))])
    (cons (moveTo B originX originY targetX targetY) '())))
|#

#| ;|useless - i need to add the 'else' statement to the 'possibleMovesForTile'... it sholud work
(define (allPossibleMovesForColor B color [L (allMovesForColor B color)]) ;RETURNS a list of all possible moves in a '(originPOS destanationPOS) structure 
  (cond                                                                   ;ignores pieces that cannot move                          
    ((empty? L) '())
    (else (append (addOriginPosToDestanation (first L)) (allPossibleMovesForColor B color (rest L))))))
|#
