% Autor:
% Datum: 26.02.2016

% Autor:
% Datum: 24.02.2016

%-----------------------------------------------------------------------------
% General Game Player environment for
% Methods of AI 2009
% Helmar Gust (c) 2009
%
% player implementation
%
% example: Charles player
%
%-----------------------------------------------------------------------------
%
% name convention:
%   <file name> = <player name>.pl
%   exported precicate:
%    ggp_<player name>_player(
%         +State,       % sorted list of atoms
%         +Role,        % role of the layer
%         -(Role:Move), % move, must be legal move
%         +Time,        % time limit in seconds
%         +MovesList    % history: list of lists of
%         )             %  the moves of the players
%                       %  (most recent first)
%
%-----------------------------------------------------------------------------
% imported predicates:
%
% gdl predicates with specified semantics:
%
%   gdl_role(?Role)
%   gdl_init(?Atom)
%   gdl_next(?Atom, +State, +Moves)
%   gdl_true(?Atom, +State)
%   gdl_does(?Role, ?Action, +State, +Moves)
%   gdl_terminal(+State)
%   gdl_goal(+Role, -Utility, +State)
%
% other useful predicates:
%
%   ggp_next_state(+State1, +Moves, -State2)
%
%-----------------------------------------------------------------------------
% gdl_init(?Atom)
%
% this predicate is true if the partial state description in 'Atom' holds in
% the initial state.
%-----------------------------------------------------------------------------
% gdl_role(?Role)
%
% this predicates enumerates the game-specific player symbols.
%-----------------------------------------------------------------------------
% gdl_legal(+Player, ?Move, +State)
%
% enumerates all possible moves for the 'Player' in the current 'State'
% a state is a list of Atoms
% a move in this case is just a move and not a structure of the form
% 'Player:Move'
%-----------------------------------------------------------------------------
% gdl_legal(?Moves, +State)
%
% enumerates all possible move combinations (one for each player)
% Moves is list of 'Player:Move' pairs. This list can use in ggp_next_state:
%    gdl_legal(Moves,S), ggp_next_state(S, Moves, S1)
% computes a possible successor state
%-----------------------------------------------------------------------------
% gdl_true(+Atom, +State)
%
% determines if the partial state description in 'Atom' holds in 'State'.
% A state is a list of Atoms
%-----------------------------------------------------------------------------
% gdl_next(?Atom, +State, +Moves)
%
% enumerates the atoms that hold in the next state after application of the
% 'Moves'.
% 'Moves' contains a list of actions that are taken from the players in the
% current turn. The list contains entries of the form 'Role:action'
% a state is a list of atoms
%-----------------------------------------------------------------------------
% ggp_next_state(+State1, +Moves, ?State2)
%
% 'State2' is the complete State description acquired by 'gdl_next'
%-----------------------------------------------------------------------------
% gdl_terminal(+State)
%
% succeeds if a state is a terminal state.
%-----------------------------------------------------------------------------
% gdl_does(?Role, ?Action, +State, +Moves)
%
% enumerates all actions players take, given a state and move decisions
%-----------------------------------------------------------------------------
% gdl_goal(+Role, -Utility, +State)
%
% succeeds on winning states for 'Role' with a 'Utility' of 100
% succeeds on draw states for with a 'Utility' of 50
% succeeds on losing states for 'Role' with a 'Utility' of 0
%
%-----------------------------------------------------------------------------
% time management
%   ggp_time_2       true if half of the time is gone
%   ggp_time_4       true if a quarter of the time is gone
%   ggp_no_time      true if time is nearly over
%   ggp_my_time(T)   holds the init/move time
%   ggp_st_time(T)   holds the start time for init/move
%   ggp_r_time(T)    holds the rest of the init/move time
%
%-----------------------------------------------------------------------------
% print debug info
%   ggp_db(+Level, +String)
%   ggp_db(+Role, +Level, +String)
%
%   for role specific printing set
%   ggp_store(Role, db_flag(+Level)),
%
%-----------------------------------------------------------------------------
% store player specific information
%  ggp_store(pred(+Role,.....))
%    example: ggp_store(depth_limit(Role,10))
%    example: ggp_store(db_level(Role,3))
%
%-----------------------------------------------------------------------------
% debugging
%  ggp_db(Level, Term)
%    print Term if Level > global debug level
%
%  ggp_db(Role, Level, Term)
%  ggp_db(Role, Level, Tag, Term)
%  ggp_db(Role, Level, Depth, Tag, Term)
%  ggp_db(Role, Level, Depth, Tag, Term, State)
%    print [Tag:]Term [and State]
%       if Level > global debug level
%       and Level > local debug level
%       stored by ggp_store(db_flag(Role,L))
%----------------------------------------------------------------------

:- module('Charles',['ggp_Charles_player'/5]).

%-------------------------------------------------------------
% initialize
%Storen alle Roles



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       TABELLE MIT ERRECHNETET ZEITEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% not(no_time)                                      :        6 inferences
%
%
%
% explore (25.02 21:40) tic_tac_toe                 :   10,850 inferences
% getStatesFromMoves_heuristic                              85 inferences
%       + (26.02 00:40) tic_tac_toe, egal welches Level
% ggp_next_state()      tic_tac_toe, "              :       80 inferences
%       + (26.02 00:40)
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



ggp_Charles_player([], Role, _Move, _Time, _MovesList):-
    %find other role and store it
    findall(R, gdl_role(R), Roles),
    select(Role,Roles,[OtherRole]),
    %Tiefenberechnung

    ggp_store(stored_role(Role, OtherRole)),
    !.

%vorlaufige version zum auswaehlen des noop moves
ggp_Charles_player(State, Role, Role:Move, _Time, _MovesList) :-

    %Das wird bei grossen Spielen eventuell auch zu problemen fuehren, wegen dem findall
    findall(X, gdl_legal(Role, X, State), [Move | []]),
    print('Auslassen'),!.




% determine a move
%alphabeta aufrufen und getTheRightMove
%ggp_Charles_player(+State,+Role,-Move,_Time,_Movelist).
ggp_Charles_player(State, Role, Move, _Time, _MovesList) :-
  ggp_store(Role, db_flag(0)),                       % set debug level to 0
  ggp_db(2,hier),
  branching_faktor(State,Role,Faktor,Children),
  print('BranchingFaktor'),print(Faktor),
  %(branching_faktor(State,Role,Faktor,[OnlyState:0:0]),
  %!,getMoveFromState(Role,State,OnlyState,Role:Move);

  time(findall(Move,gdl_legal(CurrentRole,Move,State),MoveList)),

  depth_limit(Limit,Faktor),!,

  stored_role(Role,OtherRole),


  testen(Children,Role,OtherRole, Limit),
  %unendlich([a,b,c],Role),
  computed_children(Role,ComputedChildren),
  %maximum2(ComputedChildren,MaxChildren),
  maximum(ComputedChildren,ChoosenState:Visited:Won),
  print('\n ----------------- \n'),print('MAXCHILDREN'),print('\n ----------------- \n'),print('WIE OFT BESUCHT:'),print(Visited),print('wie oft davon gewonnen:'),print(Won),print('\n ----------------- \n'),
  getTheRightMove(Role,State,ChoosenState,Move),
  print('DASHIERIST DERMOVEDENWIRNEHMEN'), print(Move),
  %alphabeta(Role:Role:State,-10000,+10000,_:_:GoodState,Value, 1),
  print('\n Dies ist der CC: \n'),
  print(ComputedChildren),
  print('\n ----------------- \n').

  %maximum2([State:Visited:Won],[State:Visited:Won]).
  %maximum2([State:Visited:Won|ComputedChildren],[MaxState:MaxVisited:MaxWon]):-
   % MaxWon>Won,
    %maximum2(ComputedChildren,[MaxState:MaxVisited:MaxWon]).
  %maximum2([State:Visited:Won|ComputedChildren],[MaxState:MaxVisited:MaxWon]):-
   % MaxWon<Won,
    %maximum2(ComputedChildren,[State:Visited:Won]).

 maximum([State:Visited:Won|Tail],MaxChild):-
   maximum(Tail,State:Visited:Won,MaxChild).

 maximum([],MaxChild,MaxChild).
 maximum([State:Visited:Won|Tail],MaxState:MaxVisited:MaxWon,MaxChild):-
   (Won>MaxWon -> maximum(Tail,State:Visited:Won,MaxChild); maximum(Tail,MaxState:MaxVisited:MaxWon,MaxChild)).



  %maximum([],EndState:EndVisited:EndWon,EndState:EndVisited:EndWon).

  %maximum([State:Visited:Won|ComputedChildren],MaxState:MaxVisited:MaxWon,EndState:EndVisited:EndWon):-
   % MaxWon<Won,
    %maximum(ComputedChildren,State:Visited:Won,EndState:EndVisited:EndWon).

 %maximum([State:Visited:Won|ComputedChildren],MaxState:MaxVisited:MaxWon,EndState:EndVisited:EndWon):-
 %   MaxWon=>Won,
 %   maximum(ComputedChildren,MaxState:MaxVisited:MaxWon,EndState:EndVisited:EndWon).




  %print('/n'),
  %print('--------------------------------'),
  %print(GoodState),
  %print('Bis hier !!!!!!!!!!!!!!!!!!!!!!!!!! b'),

  %getTheRightMove(Role,State,GoodState,Move),
  %print(Move),
  %print('/n').
  %ggp_db(2,'--------CHARLES-------'),
  %ggp_db(2,'ggp_Charles_player -> the choosen Move'),
  %ggp_db(2,Move).

depth_limit(X,_):-
  X is 20.

time_left_1_8 :-
  get_time(T),
  st_time(TS),
  my_time(MT),
  T >TS + 7*(MT / 8).

testen(Liste, Role,CurrentRole, Limit) :-
  %print('Drinnen!!!!'),
  asserta(computed_children(Role, Liste)),
  %print('Liste'),print(Liste),
  repeat,
  ignore((
    computed_children(Role, Z),
    %infinity(Z,Y,Role,CurrentRole, Limit),
    infinity(Z,X,Role,CurrentRole, Limit),
    %retract(computed_children(_,_)),   %retract
    %asserta(computed_children(Role,Z))
    %print('ALLERNEUSTEWARE'),print(X),
    once(retract(computed_children(_,_))), asserta(computed_children(Role,X))
  )),
  time_left_1_8,!.
  %time_left_1_8,!.
  %print(X).


%unendlich(Children,Role):-
%  print('Drinnen!!!!'),
  %asserta(computed_children(Role, Children)),
%  repeat,
%  ignore((
    %read
    %computed_children(Role, X)
%    infinity(X,Y),
%    infinity(Y,Z),
    %retract(computed_children(_,_)),   %retract
    %asserta(computed_children(Role,Z))
%  )),
%  no_time,!,
%  print(Z).

%infinity(_,_,_,_,_):-no_time,!.
infinity([],[],_,_,_):-!.%,print('REKURSIONSANKER FÜR INFINITY').

infinity([State:Visited:Won | T],[State:NewVisited:NewWon | Y],Role,CurrentRole,Limit) :-
   %hier fancy stuff machen
   %NewVisited is Visited + 1,
   %NewWon is Won + 1,
   %print('State VOR EXPLORE'),print(State),
   (explore(0,State, Role,CurrentRole,Limit),!,
    (NewWon is Won + 1, NewVisited is Visited + 1);
    (NewVisited is Visited + 1,NewWon is Won)),
    %print('JJEHHA DRAUSEN AUS EXPLORE'),

   infinity(T,Y,Role,CurrentRole,Limit).

not_reached_the_depth_limit(Counter,Limit):-
  Limit>Counter.%print('HEYHO').

not_a_terminal_state(State,Role):-
  not(gdl_terminal(State)).
  % not(gdl_goal(Role,0,State));not(gdl_goal(Role,50,State)).%print('NOTATERMINAL').

%explore(_Counter,State,Role,_CurrentRole,_Limit):-
%  gdl_goal(Role,X,State),X is 100,print('DAS SCHEINT UNSER UNENTSCHIEDENZUSEIN'),print(State).
%explore(Counter,_State,_Role,_,Limit):-
  %Limit<Counter,print('Limit erreicht'),print(Counter),print(Limit).
%explore(_Counter,State,_Role,_CurrentRole,_Limit):-
 %gdl_terminal(State),print('TerminalStateerreicht'),print(State).

explore(_Counter,State,Role,_CurrentRole,_Limit):-
  gdl_goal(Role,X,State),X is 100,!.%print('DAS SCHEINT UNSER GewinnerStateZUSEIN'),print(State),!.

explore(Counter, State, Role, CurrentRole,Limit) :-
  not(no_time),
  %print('DAS IST UNSER STATE'),print(State),
  not_reached_the_depth_limit(Counter,Limit),
  not_a_terminal_state(State,Role),
  findall(Move,gdl_legal(CurrentRole,Move,State),MoveList),
  %print('DAS IST UNSERE MOVELIST'),print(MoveList),
  %hieranstatt das Move in lleagal vlt CurrentRole:MOve
  length(MoveList,Length),
  %Length>0,
  random(0,Length,Number),
  %print('Number'),print(Number),
  nth0(Number,MoveList,Move_choosen),
  %print('Movechoosen'),print(Move_choosen),
  ggp_next_state(State,[CurrentRole:Move_choosen],NewState),
  %print('NewState'),print(NewState),
  Counter1 is Counter + 1,
  %print('Counter'),print(Counter),
  get_other_role(CurrentRole,OtherRole),!,
  %print('DAS IST UNSER NEUER STATE AUF DEM WEG NACH UNTEN'),
  %print('die neuen eingaben für explore'),print(Counter1),print(OtherRole),
  explore(Counter1,NewState,Role,OtherRole,Limit).






%sollte nur mit Anfangsstate aufgerufen werden und mit der eigenen Rolle,liefert Faktor
%Children von Form State:Besucht:Gewonnen
branching_faktor(State,Role,Faktor,Children):-
  findall(Move,gdl_legal(Role,Move,State),MoveList),
  length(MoveList,Faktor),
  getStatesFromMoves_heuristic(State,MoveList,Children).



%heuristic(Role,State,Val,Faktor,Counter):-
  %irgendwie berechnen wir oft wir runtergehen anhand von Faktor und Zeit
  %Depth festsetzen
 % runter(State,LimitedDepth,RatioDepth,Erfolg,Depth).


%runter(GoalState,LimitedDepth,RatioDepth+Depth,Erfolg+1,Depth):-
%  goalstate()

%runter(State,LimitedDepth,RatioDepth,Erfolg,Depth):-
%  findall(Move,gdl_legal(Role,Move,State),MoveList),
%  length(MoveList,Length),
  %nachschauen ob jetzt einer ausgelassen wird!
%  random(Length),
%  nth0(Number,MoveList,Zug),
%  ggp_next_state(State,Zug,NewState),
%  Depth1 is Depth + 1,
%  runter(NewState,LimitedDepth,RatioDepth,Erfolg,Depth1).



%getTheRightMove(+Role,+State,+Goodstate,-Move).
%holen uns den Move der zum goodstate führt.
getTheRightMove(Role,State,GoodState,Role:Move) :-
  print('IST DRIN'),print('Role'),print(Role),print('Goodstate'),print(GoodState),
  %findall(X,
  gdl_legal(Role,Move,State),
  %XList),
  %print('XList:'),print(XList),!,
  %member(Move,XList),
  print('das ist ein ausgesuchter Move'),print(Move),
  ggp_next_state(State,[Role:Move],GoodState),
  print('DAS IST DER MOVE IN GET'),print(Move),!.

%pos= role:state
%PosList= role:sate, role:state...

get_other_role2(Role, OtherRole) :-
  stored_role(Role, OtherRole), !.

get_other_role2(_, OtherRole) :-
  stored_role(OtherRole, _), !.

get_other_role(Role, OtherRole) :-
  stored_role(Role, OtherRole), !.

get_other_role(_, OtherRole) :-
  stored_role(OtherRole, _), !.


getStatesFromMoves_heuristic(_, [], []) :-
  ggp_db(2,'GetStatesFromMoves -> rekursionsanker'),!.

%State:Besucht:Gewonnen
getStatesFromMoves_heuristic(State, [Head | RestList], [StateNew:0:0 | StatesList]) :-

  ggp_next_state(State, [Role:Head], StateNew),

  getStatesFromMoves_heuristic(State, RestList, StatesList).

