% Basic AI implementation
% Simple rule-based strategy inspired by: 
% https://github.com/rlg2161/Backgammon/blob/master/agents.py

:- include('game_rules').
:- include('state_manager').

% SIMPLE AI STRATEGY
choose_move(Player, Dice, Move) :-
    findall(From-To, valid_move(Player, From, To), Moves),
    (Moves = [] 
     -> Move = none  % No moves available
     ;  evaluate_moves(Player, Moves, Dice, Scores),
        max_member(Score-BestMove, Scores),
        Move = BestMove
    ).

% EVALUATE MOVES (simple heuristic)
evaluate_moves(Player, Moves, Dice, Scores) :-
    maplist(evaluate_move(Player, Dice), Moves, Scores).

evaluate_move(Player, Dice, move(From, To), Score) :-
    % Basic strategy priorities:
    % 1. Hitting opponent blots
    % 2. Making points (creating anchors)
    % 3. Moving back checkers
    (   point(To, Opponent, 1), Opponent \= Player -> Score = 3  % Hit opponent
    ;   point(To, empty) -> Score = 2  % Make new point
    ;   Player = white, From > To -> Score = 1  % Move forward
    ;   Player = black, From < To -> Score = 1   % Move forward
    ;   Score = 0  % Default
    ).

% MINIMAX SKELETON (for future implementation)
minimax(State, Depth, Move, Eval) :-
    Depth > 0,
    findall(Child, transition(State, Child), Children),
    best_move(Children, Depth, none, -10000, Move, Eval).

best_move([], _, BestMove, BestEval, BestMove, BestEval).
best_move([Move|Moves], Depth, CurrBest, CurrEval, BestMove, BestEval) :-
    apply_move(Move),
    minimax(NewState, Depth-1, _, Eval),
    undo_move(Move),
    (Eval > CurrEval 
     -> NewBest = Move, NewEval = Eval
     ;  NewBest = CurrBest, NewEval = CurrEval
    ),
    best_move(Moves, Depth, NewBest, NewEval, BestMove, BestEval).