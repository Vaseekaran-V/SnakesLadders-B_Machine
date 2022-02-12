/* SnakesLadders
 * Author: 
 * Creation date: 2022-01-20
 */
MACHINE
    SnakesLadders

SETS
    /*Set to display the status of the game*/
    GAME_STATUS = { GameWon , GameNotOver } ;

    /*Set to display the result of a move*/
    MOVE_REPORT = { GAME_WON , GONE_DOWN_SNAKE , GONE_UP_LADDER , NORMAL_MOVE , ROLLED_TOO_HIGH_TO_FINISH , GAME_ENDED_START_NEW_ONE }

DEFINITIONS
    isPlayerInBoard ( x ) == ( x : Squares ) ;
    isSnakeMet ( x ) == ( x : Snakes ) ;
    isLadderMet ( x ) == ( x : Ladders )

CONCRETE_CONSTANTS
    /*Constants to describe the Snakes&Ladders Board*/
    Ladders , /*The starting square of a ladder*/
    Snakes , /*The starting square of a snake*/
    SnakesHeadTail , /*An array for snakes (head to tail)*/
    LaddersTopBottom , /*An array for ladders (bottom to top)*/
    Squares , /*The squares in the board*/
    End /*The ending square*/

PROPERTIES
    Squares <: NAT1 & Squares = 1 .. 100 &
    Snakes <: Squares & Snakes = { 16 , 31 , 47 , 63 , 66 , 97 } &
    Ladders <: Squares & Ladders = { 3 , 10 , 27 , 56 , 61 , 72 } &
    End <: Squares & End = { 100 } &
    SnakesHeadTail : Snakes +-> NAT1 &
    SnakesHeadTail = { 16 |-> 13 , 31 |-> 4 , 47 |-> 24 , 63 |-> 60 , 66 |-> 52 , 97 |-> 75 } &
    LaddersTopBottom : Ladders +-> NAT1 &
    LaddersTopBottom = { 3 |-> 39 , 10 |-> 12 , 27 |-> 53 , 56 |-> 84 , 61 |-> 99 , 72 |-> 90 }

ABSTRACT_VARIABLES
    playerPosition , /*The current position of the player on the board*/
    numOfSnakesMet , /*Number of snakes met by the player*/
    numOfLaddersMet , /*Number of ladders met by the player*/
    numOfTurns , /*Number of turns taken by the player*/
    playerRoute , /*The route of squares taken by the player*/
    diceRoll , /*The dice rolled by the player*/
    lastDieValue , /*The previous dice roll of the player*/
    gameStatus /*The current status of the game*/

INVARIANT
    playerPosition : Squares & playerPosition <= 100 &
    numOfSnakesMet : NAT &
    numOfLaddersMet : NAT &
    numOfTurns : NAT &
    playerRoute : seq ( INTEGER ) &
    diceRoll : NAT & diceRoll <= 6 &
    lastDieValue : NAT & lastDieValue <= 6 &
    gameStatus <: GAME_STATUS

INITIALISATION
    playerPosition := 1 ||
    numOfSnakesMet := 0 ||
    numOfLaddersMet := 0 ||
    numOfTurns := 0 ||
    playerRoute := [ 1 ] ||
    diceRoll := 0 ||
    lastDieValue := 7 ||
    gameStatus := { GameNotOver }

OPERATIONS
    /*Operation to begin the new game*/
    NewGame =
    BEGIN
        playerPosition := 1 ||
        numOfSnakesMet := 0 ||
        numOfLaddersMet := 0 ||
        numOfTurns := 0 ||
        playerRoute := [ 1 ] ||
        diceRoll := 0 ||
        lastDieValue := 0 ||
        gameStatus := { GameNotOver }
    END ;

    /*Operation to get the statistics of the game: the current position of the player, the turns taken, and the snakes and ladders met*/
    currentPosition , turnsTaken , snakesMet , laddersMet <-- GameStatistics =
    BEGIN
        currentPosition := playerPosition ||
        turnsTaken := numOfTurns ||
        snakesMet := numOfSnakesMet ||
        laddersMet := numOfLaddersMet
    END ;

    /*Getting the route the player has taken*/
    report <-- VisitedSquares =
    BEGIN
        report := playerRoute
    END ;

    /*Operation to roll the dice and move the player on the board*/
    report , currentPosition <-- Move ( diceValue ) =
    /*Setting the condition for the dice roll*/
    PRE
        diceValue : NAT1 & diceValue <= 6 &
        report : MOVE_REPORT
    THEN
        /* Check if game is not over */
        IF
            ( GameNotOver : gameStatus )
        THEN
            /* Check if player is inside the squares */
            IF
                ( isPlayerInBoard ( playerPosition + diceValue ) )
            THEN
                /*Check if current dice role wins the game */
                IF
                    ( ( playerPosition + diceValue ) : End )
                THEN
                    diceRoll := diceValue ||
                    playerPosition := playerPosition + diceValue ||
                    playerRoute := playerRoute ^ [ playerPosition + diceValue ] ||
                    numOfTurns := numOfTurns + 1 ||
                    lastDieValue := diceValue ||
                    gameStatus := { GameWon } ||
                    report := GAME_WON ||
                    currentPosition := playerPosition + diceValue
                ELSE
                    IF
                        ( not ( isSnakeMet ( playerPosition + diceValue ) ) & not ( isLadderMet ( playerPosition + diceValue ) ) )
                    THEN
                        /*Normal Move*/
                        diceRoll := diceValue ||
                        playerPosition := playerPosition + diceValue ||
                        playerRoute := playerRoute ^ [ playerPosition + diceValue ] ||
                        numOfTurns := numOfTurns + 1 ||
                        lastDieValue := diceValue ||
                        gameStatus := { GameNotOver } ||
                        report := NORMAL_MOVE ||
                        currentPosition := playerPosition + diceValue
                    ELSIF
                        /*Check if the player lands on a snake head */
                        ( isSnakeMet ( playerPosition + diceValue ) )
                    THEN
                        diceRoll := diceValue ||
                        playerPosition := SnakesHeadTail ( playerPosition + diceValue ) ||
                        playerRoute := playerRoute ^ [ playerPosition + diceValue , SnakesHeadTail ( playerPosition + diceValue ) ] ||
                        numOfTurns := numOfTurns + 1 ||
                        numOfSnakesMet := numOfSnakesMet + 1 ||
                        lastDieValue := diceValue ||
                        gameStatus := { GameNotOver } ||
                        report := GONE_DOWN_SNAKE ||
                        currentPosition := SnakesHeadTail ( playerPosition + diceValue )
                    ELSIF
                        /*Check if the player lands on a ladder bottom*/
                        ( isLadderMet ( playerPosition + diceValue ) )
                    THEN
                        diceRoll := diceValue ||
                        playerPosition := LaddersTopBottom ( playerPosition + diceValue ) ||
                        playerRoute := playerRoute ^ [ playerPosition + diceValue , LaddersTopBottom ( playerPosition + diceValue ) ] ||
                        numOfTurns := numOfTurns + 1 ||
                        numOfLaddersMet := numOfLaddersMet + 1 ||
                        lastDieValue := diceValue ||
                        gameStatus := { GameNotOver } ||
                        report := GONE_UP_LADDER ||
                        currentPosition := LaddersTopBottom ( playerPosition + diceValue )
                    END
                END
            ELSIF
                /* Check if player has gone out of board with the dice roll */
                ( not ( isPlayerInBoard ( playerPosition + diceValue ) ) )
            THEN
                diceRoll := diceValue ||
                gameStatus := { GameNotOver } ||
                playerPosition := playerPosition ||
                numOfTurns := numOfTurns + 1 ||
                lastDieValue := diceValue ||
                report := ROLLED_TOO_HIGH_TO_FINISH ||
                currentPosition := playerPosition
            END
        ELSIF
            ( GameWon : gameStatus )
        THEN
            report := GAME_ENDED_START_NEW_ONE ||
            currentPosition := playerPosition
        END
    END
END
