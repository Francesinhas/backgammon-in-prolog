% AI implementation

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
:- discontiguous can_leave_bar_state/0.
:- discontiguous can_leave_bar_state2/0.
:- discontiguous entry_range/3.

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
    (   can_bear_off(Player)
    ->  findall(Point, (point(Point, Player, C), C > 0, validate_and_get_bear_off_die(Player, Point, _)), Points),
        maplist(wrap_bear_off_move(Player), Points, ScoredMoves)
    ;   ScoredMoves = []
    ).

select_best_move([], none).
select_best_move(ScoredMoves, Move) :-
    max_member(_Score-Move, ScoredMoves).


wrap_and_evaluate(Player, From-To, Score-move(From, To)) :-
    evaluate_move(Player, move(From, To), Score).

evaluate_move(Player, move(From, To), Score) :-
    opponent(Player, Opponent),

    % Case 1: Hitting an opponent's blot. This is a top priority.
    (   point(To, Opponent, 1) ->
        (   point(To, Player, _) ->
            Score is 20 + 5  % Hit and cover: Very safe, high reward.
        ;   \+ is_blot_hittable(To, Player) ->
            Score is 20 + 3  % Hit and run: Land on a new, safe blot.
        ;   Score is 20      % Hit but leave a hittable blot. Still good.
        )

    % Case 2: Making a new point (securing a blot).
    ;   point(To, Player, 1) ->
        prime_contribution_bonus(To, Player, PrimeBonus),
        Score is 15 + PrimeBonus % Base score for making a point, plus prime bonus.

    % Case 3: Moving to an empty point (creating a new blot).
    ;   \+ point(To, _, _) ->
        (   is_blot_hittable(To, Player) ->
            Score is 1 % Heavily penalize creating an unsafe blot.
        ;   Score is 5 % Creating a safe blot is a reasonable move.
        )

    % Case 4: Adding to an already established, safe point.
    ;   point(To, Player, Count), Count > 1 ->
        (   Count > 4 -> Score is 2 % Penalize inefficient stacking.
        ;   Score is 4 % Standard move, safe but not creating new structure.
        )
    ;   Score = 0 % Default case
    ),

    % Apply penalty for breaking a made point, unless it's for a hit.
    (   point(From, Player, 2), \+ point(To, Opponent, 1) ->
        FinalScore is Score - 3
    ;   FinalScore is Score
    ).


wrap_bar_move(Player, To, Score-move(bar, To)) :-
    evaluate_bar_entry(Player, To, Score).

evaluate_bar_entry(Player, To, Score) :-
    opponent(Player, Opponent),
    % A higher score range is used to emphasize that any bar move is critical.
    (   point(To, Opponent, 1) -> Score = 25  % Hit on entry: best case.
    ;   point(To, Player, _) -> Score = 20    % Enter safely on own point.
    ;   \+ point(To, _, _) ->                 % Enter to an empty point.
        (   is_blot_hittable(To, Player) ->
            Score is 5  % Unsafe entry, but better than no move.
        ;   Score is 10 % Safe entry, but creates a blot.
        )
    ;   Score = 0
    ).


wrap_bear_off_move(Player, Point, Score-move(Point, off)) :-
    evaluate_bear_off(Player, Point, Score).

evaluate_bear_off(Player, Point, Score) :-
    % Base score of 30 to prioritize bearing off over all other moves.
    % Adds a small bonus for bearing off from higher points to be more efficient.
    (   Player = white ->
        Score is 30 + (Point / 10.0)
    ;   Player = black ->
        Score is 30 + ((25 - Point) / 10.0)
    ).

choose_move_with_dice_old(Player, Move) :-  % not taking bearing off and bar into account - not used!
    findall(From-To, valid_move_with_dice(Player, From, To), Moves),
    (Moves = [] 
        -> Move = none  % No moves available
        ;  maplist(wrap_and_evaluate(Player), Moves, Scores),
        max_member(Score-BestMove, Scores),
        Move = BestMove
    ).

% helper 
opponent(white, black).
opponent(black, white).

is_blot_hittable(Point, Player) :-
    opponent(Player, Opponent),
    findall(D, (
        between(1, 6, D),
        (   Opponent = white -> CheckPoint is Point + D
        ;   Opponent = black -> CheckPoint is Point - D
        ),
        between(1, 24, CheckPoint), % Ensure the point is on the board
        point(CheckPoint, Opponent, C), C > 0
    ), Hits),
    Hits \= []. % Succeeds if the list of possible hits is not empty.

prime_contribution_bonus(Point, Player, 2) :-
    Adj1 is Point - 1,
    Adj2 is Point + 1,
    (   (between(1, 24, Adj1), point(Adj1, Player, C1), C1 > 0)
    ;   (between(1, 24, Adj2), point(Adj2, Player, C2), C2 > 0)
    ), !.
prime_contribution_bonus(_, _, 0).


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
