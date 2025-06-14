% Basic AI implementation

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

:- include('state_manager').

% SIMPLE AI STRATEGY
choose_move_with_dice(Player, Move) :-
    generate_ai_moves(Player, ScoredMoves),
    select_best_move(ScoredMoves, Move).

generate_ai_moves(Player, AllMoves) :-
    bar(Player, Count),
    (   Count > 0
    ->  generate_bar_entry_moves(Player, BarMoves),
        AllMoves = BarMoves
    ;   generate_normal_moves(Player, NormalMoves),
        generate_bear_off_moves(Player, BearOffMoves),
        append(NormalMoves, BearOffMoves, AllMoves)
    ).


generate_bar_entry_moves(Player, ScoredMoves) :-
    findall(To, can_move_from_bar_with_dice(Player, To), EntryPoints),
    maplist(wrap_bar_move(Player), EntryPoints, ScoredMoves).

generate_normal_moves(Player, ScoredMoves) :-
    findall(From-To, valid_move_with_dice(Player, From, To), Moves),
    maplist(wrap_and_evaluate(Player), Moves, ScoredMoves).

generate_bear_off_moves(Player, ScoredMoves) :-
    can_bear_off(Player),
    findall(Point, (point(Point, Player, C), C > 0, validate_and_get_bear_off_die(Player, Point, _)), Points),
    maplist(wrap_bear_off_move(Player), Points, ScoredMoves).


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


wrap_bear_off_move(Player, Point, Score-move(Point, off)) :-
    evaluate_bear_off(Player, Point, Score).

evaluate_bear_off(_Player, _Point, 5).  % 5p. bearing off a piece
% todo: use Point to assign score based on distance






choose_move_with_dice_old(Player, Move) :-  % not taking bearing off and bar into account - not used!
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
