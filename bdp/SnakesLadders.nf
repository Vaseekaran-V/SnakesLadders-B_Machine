Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(SnakesLadders))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(SnakesLadders))==(Machine(SnakesLadders));
  Level(Machine(SnakesLadders))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(SnakesLadders)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(SnakesLadders))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(SnakesLadders))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(SnakesLadders))==(?);
  List_Includes(Machine(SnakesLadders))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(SnakesLadders))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(SnakesLadders))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(SnakesLadders))==(?);
  Context_List_Variables(Machine(SnakesLadders))==(?);
  Abstract_List_Variables(Machine(SnakesLadders))==(?);
  Local_List_Variables(Machine(SnakesLadders))==(gameStatus,lastDieValue,diceRoll,playerRoute,numOfTurns,numOfLaddersMet,numOfSnakesMet,playerPosition);
  List_Variables(Machine(SnakesLadders))==(gameStatus,lastDieValue,diceRoll,playerRoute,numOfTurns,numOfLaddersMet,numOfSnakesMet,playerPosition);
  External_List_Variables(Machine(SnakesLadders))==(gameStatus,lastDieValue,diceRoll,playerRoute,numOfTurns,numOfLaddersMet,numOfSnakesMet,playerPosition)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(SnakesLadders))==(?);
  Abstract_List_VisibleVariables(Machine(SnakesLadders))==(?);
  External_List_VisibleVariables(Machine(SnakesLadders))==(?);
  Expanded_List_VisibleVariables(Machine(SnakesLadders))==(?);
  List_VisibleVariables(Machine(SnakesLadders))==(?);
  Internal_List_VisibleVariables(Machine(SnakesLadders))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(SnakesLadders))==(btrue);
  Gluing_List_Invariant(Machine(SnakesLadders))==(btrue);
  Expanded_List_Invariant(Machine(SnakesLadders))==(btrue);
  Abstract_List_Invariant(Machine(SnakesLadders))==(btrue);
  Context_List_Invariant(Machine(SnakesLadders))==(btrue);
  List_Invariant(Machine(SnakesLadders))==(playerPosition: Squares & playerPosition<=100 & numOfSnakesMet: NAT & numOfLaddersMet: NAT & numOfTurns: NAT & playerRoute: seq(INTEGER) & diceRoll: NAT & diceRoll<=6 & lastDieValue: NAT & lastDieValue<=6 & gameStatus <: GAME_STATUS)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(SnakesLadders))==(btrue);
  Abstract_List_Assertions(Machine(SnakesLadders))==(btrue);
  Context_List_Assertions(Machine(SnakesLadders))==(btrue);
  List_Assertions(Machine(SnakesLadders))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(SnakesLadders))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(SnakesLadders))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(SnakesLadders))==(playerPosition,numOfSnakesMet,numOfLaddersMet,numOfTurns,playerRoute,diceRoll,lastDieValue,gameStatus:=1,0,0,0,[1],0,7,{GameNotOver});
  Context_List_Initialisation(Machine(SnakesLadders))==(skip);
  List_Initialisation(Machine(SnakesLadders))==(playerPosition:=1 || numOfSnakesMet:=0 || numOfLaddersMet:=0 || numOfTurns:=0 || playerRoute:=[1] || diceRoll:=0 || lastDieValue:=7 || gameStatus:={GameNotOver})
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(SnakesLadders))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(SnakesLadders))==(btrue);
  List_Constraints(Machine(SnakesLadders))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(SnakesLadders))==(NewGame,GameStatistics,VisitedSquares,Move);
  List_Operations(Machine(SnakesLadders))==(NewGame,GameStatistics,VisitedSquares,Move)
END
&
THEORY ListInputX IS
  List_Input(Machine(SnakesLadders),NewGame)==(?);
  List_Input(Machine(SnakesLadders),GameStatistics)==(?);
  List_Input(Machine(SnakesLadders),VisitedSquares)==(?);
  List_Input(Machine(SnakesLadders),Move)==(diceValue)
END
&
THEORY ListOutputX IS
  List_Output(Machine(SnakesLadders),NewGame)==(?);
  List_Output(Machine(SnakesLadders),GameStatistics)==(currentPosition,turnsTaken,snakesMet,laddersMet);
  List_Output(Machine(SnakesLadders),VisitedSquares)==(report);
  List_Output(Machine(SnakesLadders),Move)==(report,currentPosition)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(SnakesLadders),NewGame)==(NewGame);
  List_Header(Machine(SnakesLadders),GameStatistics)==(currentPosition,turnsTaken,snakesMet,laddersMet <-- GameStatistics);
  List_Header(Machine(SnakesLadders),VisitedSquares)==(report <-- VisitedSquares);
  List_Header(Machine(SnakesLadders),Move)==(report,currentPosition <-- Move(diceValue))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(SnakesLadders),NewGame)==(btrue);
  List_Precondition(Machine(SnakesLadders),GameStatistics)==(btrue);
  List_Precondition(Machine(SnakesLadders),VisitedSquares)==(btrue);
  List_Precondition(Machine(SnakesLadders),Move)==(diceValue: NAT1 & diceValue<=6 & report: MOVE_REPORT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(SnakesLadders),Move)==(diceValue: NAT1 & diceValue<=6 & report: MOVE_REPORT | GameNotOver: gameStatus ==> (playerPosition+diceValue: Squares ==> (playerPosition+diceValue: End ==> diceRoll,playerPosition,playerRoute,numOfTurns,lastDieValue,gameStatus,report,currentPosition:=diceValue,playerPosition+diceValue,playerRoute^[playerPosition+diceValue],numOfTurns+1,diceValue,{GameWon},GAME_WON,playerPosition+diceValue [] not(playerPosition+diceValue: End) ==> (not(playerPosition+diceValue: Snakes) & not(playerPosition+diceValue: Ladders) ==> diceRoll,playerPosition,playerRoute,numOfTurns,lastDieValue,gameStatus,report,currentPosition:=diceValue,playerPosition+diceValue,playerRoute^[playerPosition+diceValue],numOfTurns+1,diceValue,{GameNotOver},NORMAL_MOVE,playerPosition+diceValue [] not(not(playerPosition+diceValue: Snakes) & not(playerPosition+diceValue: Ladders)) ==> (playerPosition+diceValue: Snakes ==> diceRoll,playerPosition,playerRoute,numOfTurns,numOfSnakesMet,lastDieValue,gameStatus,report,currentPosition:=diceValue,SnakesHeadTail(playerPosition+diceValue),playerRoute^[playerPosition+diceValue,SnakesHeadTail(playerPosition+diceValue)],numOfTurns+1,numOfSnakesMet+1,diceValue,{GameNotOver},GONE_DOWN_SNAKE,SnakesHeadTail(playerPosition+diceValue) [] not(playerPosition+diceValue: Snakes) ==> (playerPosition+diceValue: Ladders ==> diceRoll,playerPosition,playerRoute,numOfTurns,numOfLaddersMet,lastDieValue,gameStatus,report,currentPosition:=diceValue,LaddersTopBottom(playerPosition+diceValue),playerRoute^[playerPosition+diceValue,LaddersTopBottom(playerPosition+diceValue)],numOfTurns+1,numOfLaddersMet+1,diceValue,{GameNotOver},GONE_UP_LADDER,LaddersTopBottom(playerPosition+diceValue) [] not(playerPosition+diceValue: Ladders) ==> skip)))) [] not(playerPosition+diceValue: Squares) ==> (not(playerPosition+diceValue: Squares) ==> diceRoll,gameStatus,playerPosition,numOfTurns,lastDieValue,report,currentPosition:=diceValue,{GameNotOver},playerPosition,numOfTurns+1,diceValue,ROLLED_TOO_HIGH_TO_FINISH,playerPosition [] not(not(playerPosition+diceValue: Squares)) ==> skip)) [] not(GameNotOver: gameStatus) ==> (GameWon: gameStatus ==> report,currentPosition:=GAME_ENDED_START_NEW_ONE,playerPosition [] not(GameWon: gameStatus) ==> skip));
  Expanded_List_Substitution(Machine(SnakesLadders),VisitedSquares)==(btrue | report:=playerRoute);
  Expanded_List_Substitution(Machine(SnakesLadders),GameStatistics)==(btrue | currentPosition,turnsTaken,snakesMet,laddersMet:=playerPosition,numOfTurns,numOfSnakesMet,numOfLaddersMet);
  Expanded_List_Substitution(Machine(SnakesLadders),NewGame)==(btrue | playerPosition,numOfSnakesMet,numOfLaddersMet,numOfTurns,playerRoute,diceRoll,lastDieValue,gameStatus:=1,0,0,0,[1],0,0,{GameNotOver});
  List_Substitution(Machine(SnakesLadders),NewGame)==(playerPosition:=1 || numOfSnakesMet:=0 || numOfLaddersMet:=0 || numOfTurns:=0 || playerRoute:=[1] || diceRoll:=0 || lastDieValue:=0 || gameStatus:={GameNotOver});
  List_Substitution(Machine(SnakesLadders),GameStatistics)==(currentPosition:=playerPosition || turnsTaken:=numOfTurns || snakesMet:=numOfSnakesMet || laddersMet:=numOfLaddersMet);
  List_Substitution(Machine(SnakesLadders),VisitedSquares)==(report:=playerRoute);
  List_Substitution(Machine(SnakesLadders),Move)==(IF GameNotOver: gameStatus THEN IF playerPosition+diceValue: Squares THEN IF playerPosition+diceValue: End THEN diceRoll:=diceValue || playerPosition:=playerPosition+diceValue || playerRoute:=playerRoute^[playerPosition+diceValue] || numOfTurns:=numOfTurns+1 || lastDieValue:=diceValue || gameStatus:={GameWon} || report:=GAME_WON || currentPosition:=playerPosition+diceValue ELSE IF not(playerPosition+diceValue: Snakes) & not(playerPosition+diceValue: Ladders) THEN diceRoll:=diceValue || playerPosition:=playerPosition+diceValue || playerRoute:=playerRoute^[playerPosition+diceValue] || numOfTurns:=numOfTurns+1 || lastDieValue:=diceValue || gameStatus:={GameNotOver} || report:=NORMAL_MOVE || currentPosition:=playerPosition+diceValue ELSIF playerPosition+diceValue: Snakes THEN diceRoll:=diceValue || playerPosition:=SnakesHeadTail(playerPosition+diceValue) || playerRoute:=playerRoute^[playerPosition+diceValue,SnakesHeadTail(playerPosition+diceValue)] || numOfTurns:=numOfTurns+1 || numOfSnakesMet:=numOfSnakesMet+1 || lastDieValue:=diceValue || gameStatus:={GameNotOver} || report:=GONE_DOWN_SNAKE || currentPosition:=SnakesHeadTail(playerPosition+diceValue) ELSIF playerPosition+diceValue: Ladders THEN diceRoll:=diceValue || playerPosition:=LaddersTopBottom(playerPosition+diceValue) || playerRoute:=playerRoute^[playerPosition+diceValue,LaddersTopBottom(playerPosition+diceValue)] || numOfTurns:=numOfTurns+1 || numOfLaddersMet:=numOfLaddersMet+1 || lastDieValue:=diceValue || gameStatus:={GameNotOver} || report:=GONE_UP_LADDER || currentPosition:=LaddersTopBottom(playerPosition+diceValue) END END ELSIF not(playerPosition+diceValue: Squares) THEN diceRoll:=diceValue || gameStatus:={GameNotOver} || playerPosition:=playerPosition || numOfTurns:=numOfTurns+1 || lastDieValue:=diceValue || report:=ROLLED_TOO_HIGH_TO_FINISH || currentPosition:=playerPosition END ELSIF GameWon: gameStatus THEN report:=GAME_ENDED_START_NEW_ONE || currentPosition:=playerPosition END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(SnakesLadders))==(Ladders,Snakes,SnakesHeadTail,LaddersTopBottom,Squares,End);
  Inherited_List_Constants(Machine(SnakesLadders))==(?);
  List_Constants(Machine(SnakesLadders))==(Ladders,Snakes,SnakesHeadTail,LaddersTopBottom,Squares,End)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(SnakesLadders),GAME_STATUS)==({GameWon,GameNotOver});
  Context_List_Enumerated(Machine(SnakesLadders))==(?);
  Context_List_Defered(Machine(SnakesLadders))==(?);
  Context_List_Sets(Machine(SnakesLadders))==(?);
  List_Valuable_Sets(Machine(SnakesLadders))==(?);
  Inherited_List_Enumerated(Machine(SnakesLadders))==(?);
  Inherited_List_Defered(Machine(SnakesLadders))==(?);
  Inherited_List_Sets(Machine(SnakesLadders))==(?);
  List_Enumerated(Machine(SnakesLadders))==(GAME_STATUS,MOVE_REPORT);
  List_Defered(Machine(SnakesLadders))==(?);
  List_Sets(Machine(SnakesLadders))==(GAME_STATUS,MOVE_REPORT);
  Set_Definition(Machine(SnakesLadders),MOVE_REPORT)==({GAME_WON,GONE_DOWN_SNAKE,GONE_UP_LADDER,NORMAL_MOVE,ROLLED_TOO_HIGH_TO_FINISH,GAME_ENDED_START_NEW_ONE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(SnakesLadders))==(?);
  Expanded_List_HiddenConstants(Machine(SnakesLadders))==(?);
  List_HiddenConstants(Machine(SnakesLadders))==(?);
  External_List_HiddenConstants(Machine(SnakesLadders))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(SnakesLadders))==(btrue);
  Context_List_Properties(Machine(SnakesLadders))==(btrue);
  Inherited_List_Properties(Machine(SnakesLadders))==(btrue);
  List_Properties(Machine(SnakesLadders))==(Squares <: NAT1 & Squares = 1..100 & Snakes <: Squares & Snakes = {16,31,47,63,66,97} & Ladders <: Squares & Ladders = {3,10,27,56,61,72} & End <: Squares & End = {100} & SnakesHeadTail: Snakes +-> NAT1 & SnakesHeadTail = {16|->13,31|->4,47|->24,63|->60,66|->52,97|->75} & LaddersTopBottom: Ladders +-> NAT1 & LaddersTopBottom = {3|->39,10|->12,27|->53,56|->84,61|->99,72|->90} & GAME_STATUS: FIN(INTEGER) & not(GAME_STATUS = {}) & MOVE_REPORT: FIN(INTEGER) & not(MOVE_REPORT = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(SnakesLadders),NewGame)==(?);
  List_ANY_Var(Machine(SnakesLadders),GameStatistics)==(?);
  List_ANY_Var(Machine(SnakesLadders),VisitedSquares)==(?);
  List_ANY_Var(Machine(SnakesLadders),Move)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(SnakesLadders)) == (Ladders,Snakes,SnakesHeadTail,LaddersTopBottom,Squares,End,GAME_STATUS,MOVE_REPORT,GameWon,GameNotOver,GAME_WON,GONE_DOWN_SNAKE,GONE_UP_LADDER,NORMAL_MOVE,ROLLED_TOO_HIGH_TO_FINISH,GAME_ENDED_START_NEW_ONE | ? | gameStatus,lastDieValue,diceRoll,playerRoute,numOfTurns,numOfLaddersMet,numOfSnakesMet,playerPosition | ? | NewGame,GameStatistics,VisitedSquares,Move | ? | ? | ? | SnakesLadders);
  List_Of_HiddenCst_Ids(Machine(SnakesLadders)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SnakesLadders)) == (Ladders,Snakes,SnakesHeadTail,LaddersTopBottom,Squares,End);
  List_Of_VisibleVar_Ids(Machine(SnakesLadders)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SnakesLadders)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(SnakesLadders)) == (Type(GAME_STATUS) == Cst(SetOf(etype(GAME_STATUS,0,1)));Type(MOVE_REPORT) == Cst(SetOf(etype(MOVE_REPORT,0,5))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(SnakesLadders)) == (Type(GameWon) == Cst(etype(GAME_STATUS,0,1));Type(GameNotOver) == Cst(etype(GAME_STATUS,0,1));Type(GAME_WON) == Cst(etype(MOVE_REPORT,0,5));Type(GONE_DOWN_SNAKE) == Cst(etype(MOVE_REPORT,0,5));Type(GONE_UP_LADDER) == Cst(etype(MOVE_REPORT,0,5));Type(NORMAL_MOVE) == Cst(etype(MOVE_REPORT,0,5));Type(ROLLED_TOO_HIGH_TO_FINISH) == Cst(etype(MOVE_REPORT,0,5));Type(GAME_ENDED_START_NEW_ONE) == Cst(etype(MOVE_REPORT,0,5));Type(Ladders) == Cst(SetOf(btype(INTEGER,"[Ladders","]Ladders")));Type(Snakes) == Cst(SetOf(btype(INTEGER,"[Snakes","]Snakes")));Type(SnakesHeadTail) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(LaddersTopBottom) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(Squares) == Cst(SetOf(btype(INTEGER,"[Squares","]Squares")));Type(End) == Cst(SetOf(btype(INTEGER,"[End","]End"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(SnakesLadders)) == (Type(gameStatus) == Mvl(SetOf(etype(GAME_STATUS,?,?)));Type(lastDieValue) == Mvl(btype(INTEGER,?,?));Type(diceRoll) == Mvl(btype(INTEGER,?,?));Type(playerRoute) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(numOfTurns) == Mvl(btype(INTEGER,?,?));Type(numOfLaddersMet) == Mvl(btype(INTEGER,?,?));Type(numOfSnakesMet) == Mvl(btype(INTEGER,?,?));Type(playerPosition) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(SnakesLadders)) == (Type(Move) == Cst(etype(MOVE_REPORT,?,?)*btype(INTEGER,?,?),btype(INTEGER,?,?));Type(VisitedSquares) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)),No_type);Type(GameStatistics) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type);Type(NewGame) == Cst(No_type,No_type));
  Observers(Machine(SnakesLadders)) == (Type(VisitedSquares) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)),No_type);Type(GameStatistics) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?),No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
