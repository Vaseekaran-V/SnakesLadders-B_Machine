/* SnakesLadders
 * Author: 
 * Creation date: 2022-01-20
 */
MACHINE
    SnakesLadders
    
SETS
    GAME_STATUS = {GameWon, GameLost, GameNotOver};
    
    MOVE_REPORT = {GAME_WON, GONE_DOWN_SNAKE, GONE_UP_LADDER, NORMAL_MOVE, ROLLED_TOO_HIGH_TO_FINISH, GAME_ENDED_START_NEW_ONE}
    
DEFINITIONS
    isPlayerInBoard(x) == (x: Squares);
    isSnakeMet(x) == (x: Snakes);
    isLadderMet(x) == (x: Ladders)
    
CONSTANTS
    Ladders,
    Snakes,
    SnakesHeadTail,
    LaddersTopBottom,
    Squares,
    Start,
    End
    
PROPERTIES
    Squares <: NAT1 & Squares = 1..100 &
    Snakes<: NAT1 & Snakes = {16, 31, 47, 63, 66, 97} &
    Ladders<: NAT1 & Ladders = {3, 10, 27, 56, 61, 72} &
    Start<: NAT1 & Start = {1} &
    End<: NAT1 & End = {100} &
    SnakesHeadTail: Snakes +-> NAT1 & 
    SnakesHeadTail = {16 |->13, 31 |-> 4, 47 |->24, 63 |->60, 66 |->52, 97 |->75} &
    LaddersTopBottom: Ladders +-> NAT1 & 
    LaddersTopBottom = {3 |->39, 10 |-> 12, 27 |->53, 56 |->84, 61 |->99, 72 |->90}
    
VARIABLES
    playerPosition,
    numOfSnakesMet,
    numOfLaddersMet,
    numOfTurns,
    playerRoute,
    diceRoll,
    lastDieValue,
    gameStatus
    
INVARIANT
    playerPosition: NAT1 & playerPosition <= 100 &
    numOfSnakesMet: NAT &
    numOfLaddersMet: NAT &
    numOfTurns: NAT &
    playerRoute: seq(INTEGER) &
    diceRoll: NAT & diceRoll <= 6 &
    lastDieValue: NAT & lastDieValue <= 6 &
    gameStatus <: GAME_STATUS
    
INITIALISATION
    playerPosition := 1 ||
    numOfSnakesMet := 0 ||
    numOfLaddersMet := 0 ||
    numOfTurns := 0 ||
    playerRoute := [1] ||
    diceRoll := 0 ||
    lastDieValue := 7 ||
    gameStatus := {GameNotOver}
    
OPERATIONS
    
    NewGame =
    
    BEGIN
    
        playerPosition := 1 ||
        numOfSnakesMet := 0 ||
        numOfLaddersMet := 0 ||
        numOfTurns := 0 ||
        playerRoute := [1] ||
        diceRoll := 0 ||
        lastDieValue := 0 ||
        gameStatus := {GameNotOver}
        
    END;
    
    currentPosition, turnsTaken, snakesMet, laddersMet <--  GameStatistics =
    
    BEGIN
        
        currentPosition := playerPosition ||
        turnsTaken := numOfTurns ||
        snakesMet := numOfSnakesMet ||
        laddersMet := numOfLaddersMet
        
    END;
    
    report <-- VisitedSquares =
    
    BEGIN
        
        report := playerRoute
        
    END;
    
    report, currentPosition <-- Move(diceValue) =
    
    PRE
        
        diceValue: NAT1 & diceValue <= 6 &
        report: MOVE_REPORT
        
    THEN
        
        
        /* Check if game is not over */
        IF
            (GameNotOver: gameStatus)
        THEN
            
            /* Check if player is inside the squares */
            IF
                
                (isPlayerInBoard(playerPosition + diceValue))
            THEN
                /*Check if current dice role wins the game */
                IF
                    ( (playerPosition + diceValue): End )
                THEN
                    diceRoll := diceValue ||
                    playerPosition := playerPosition + diceValue ||
                    playerRoute := playerRoute ^ [ playerPosition + diceValue ] ||
                    numOfTurns := numOfTurns + 1 ||
                    lastDieValue := diceValue ||
                    gameStatus := {GameWon} ||
                    report := GAME_WON ||
                    currentPosition := playerPosition + diceValue
                    
                ELSE
                    /*Normal Move*/
                    diceRoll := diceValue ||
                    playerPosition := playerPosition + diceValue ||
                    playerRoute := playerRoute ^ [ playerPosition + diceValue ] ||
                    numOfTurns := numOfTurns + 1 ||
                    lastDieValue := diceValue ||
                    gameStatus := {GameNotOver} ||
                    report := NORMAL_MOVE ||
                    currentPosition := playerPosition + diceValue
                    
                    
                    
                END
            ELSIF
                /* Check if player has gone out of board with the dice roll */
                (not(isPlayerInBoard(playerPosition + diceValue)))
            THEN
                diceRoll := diceValue ||
                gameStatus := {GameNotOver} ||
                playerPosition := playerPosition ||
                playerRoute := playerRoute ^ [playerPosition] ||
                numOfTurns := numOfTurns + 1 ||
                lastDieValue := diceValue ||
                report := ROLLED_TOO_HIGH_TO_FINISH ||
                currentPosition := playerPosition
            END
        ELSIF
            (GameWon: gameStatus)
        THEN
            report := GAME_ENDED_START_NEW_ONE ||
            currentPosition := playerPosition
        END
    END


END
