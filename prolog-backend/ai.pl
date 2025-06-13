% Basic AI implementation
% Simple rule-based strategy inspired by: 

:- discontiguous use_die/1.
:- discontiguous land_on/2.
:- discontiguous apply_move/3.
:- discontiguous move_from_bar/2.
:- discontiguous bear_off_piece/2.
:- discontiguous update_bar/2.
:- discontiguous update_off/2.
:- discontiguous state_to_list/1.
:- discontiguous entry_point_length/1.
:- discontiguous move_length/1.
:- discontiguous bear_off_length/1.

:- include('game_rules').
:- include('state_manager').

% SIMPLE AI STRATEGY
choose_move_with_dice_good(Player, Move) :-
    generate_ai_moves(Player, ScoredMoves),
    select_best_move(ScoredMoves, Move).

generate_ai_moves(Player, ScoredMoves) :-
    bar(Player, Count),
    (   Count > 0
    ->  generate_bar_entry_moves(Player, ScoredMoves)
    ;   generate_normal_moves(Player, ScoredMoves)
    ).

generate_bar_entry_moves(Player, ScoredMoves) :-
    findall(To, move_from_bar_with_dice(Player, To), EntryPoints),
    maplist(wrap_bar_move(Player), EntryPoints, ScoredMoves).

generate_normal_moves(Player, ScoredMoves) :-
    findall(From-To, valid_move_with_dice(Player, From, To), Moves),
    maplist(wrap_and_evaluate(Player), Moves, ScoredMoves).

select_best_move([], none).
select_best_move(ScoredMoves, Move) :-
    max_member(_Score-Move, ScoredMoves).


wrap_and_evaluate(Player, From-To, Score-move(From, To)) :-
    evaluate_move(Player, move(From, To), Score).

evaluate_move(Player, move(From, To), Score) :-
    % Basic strategy priorities:
    % 1. Hitting opponent blots
    % 2. Making points (creating anchors)
    % 3. Moving back checkers
    (   point(To, Opponent, 1), Opponent \= Player -> Score = 3
    ;   point(To, Player, _) -> Score = 2
    ;   \+ point(To, _, _) -> Score = 1
    ;   Score = 0
    ).


wrap_bar_move(Player, To, Score-move(bar, To)) :-
    evaluate_bar_entry(Player, To, Score).

evaluate_bar_entry(Player, To, Score) :-
    % Basic strategy priorities:
    % 3p. Hitting opponent blots
    % 2p. Making points (creating anchors)
    % 1p. Exposing single blot
    (   point(To, Opponent, 1), Opponent \= Player -> Score = 3  
    ;   point(To, Player, _) -> Score = 2 
    ;   \+ point(To, _, _) -> Score = 1 
    ;   Score = 0
    ).






choose_move_with_dice(Player, Move) :-  % not taking bearing off and bar into account - not used!
    findall(From-To, valid_move_with_dice(Player, From, To), Moves),
    (Moves = [] 
        -> Move = none  % No moves available
        ;  maplist(wrap_and_evaluate(Player), Moves, Scores),
        max_member(Score-BestMove, Scores),
        Move = BestMove
    ).


% MINIMAX SKELETON (for future implementation)
% minimax(State, Depth, Move, Eval) :-
%     Depth > 0,
%     findall(Child, transition(State, Child), Children),
%     best_move(Children, Depth, none, -10000, Move, Eval).

% best_move([], _, BestMove, BestEval, BestMove, BestEval).
% best_move([Move|Moves], Depth, CurrBest, CurrEval, BestMove, BestEval) :-
%     apply_move(Move),
%     minimax(NewState, Depth-1, _, Eval),
%     undo_move(Move),
%     (Eval > CurrEval 
%      -> NewBest = Move, NewEval = Eval
%      ;  NewBest = CurrBest, NewEval = CurrEval
%     ),
%     best_move(Moves, Depth, NewBest, NewEval, BestMove, BestEval).
