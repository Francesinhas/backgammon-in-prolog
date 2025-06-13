% Main file that aggregates commands for updating the state

% Other useful predicates:
%   initial_state/0
%   current_dice/1 
%   state_to_list/1 - [Points, Bar, Off]
%   dice_roll/1

% necessary supressions for swi runtime

% :- include('game_rules'). % already included in both state_manager and ai
:- include('state_manager').
:- include('ai').

get_best_move(Player, BestMove) :-
    choose_move_with_dice(Player, Move),
    (   Move == none
    ->  BestMove = false
    ;   BestMove = Move
    ).

ai_turn(Player) :-
    choose_move_with_dice(Player, move(bar, To)),
    perform_move_from_bar(Player, To).

ai_turn(Player) :-
    choose_move_with_dice(Player, move(From, To)),
    perform_move(Player, From, To).

% Fallback when no move is available, open for extension
ai_turn(Player) :-
    choose_move_with_dice(Player, none),
    % format("No available moves for ~w~n", [Player]).
    false.

perform_move(Player, From, To) :-
    valid_move_with_dice(Player, From, To),
    apply_move(Player, From, To),
    move_length(Player, From, To, L),
    use_die(L).

perform_move_from_bar(Player, To) :-
    bar(Player, BarCount), BarCount > 0,
    entry_point_length(Player, To, L),
    move_from_bar_with_dice(Player, To),
    use_die(L).

perform_bear_off(Player, Point) :-
    % 1. Validate the move using the new logic and get the die that will be used.
    validate_and_get_bear_off_die(Player, Point, UsedDie),
    
    % 2. Apply the state change (remove checker from board and add to 'off').
    bear_off_piece(Player, Point),
    
    % 3. Consume the correct die from the current roll.
    use_die(UsedDie).

perform_move_from_bar_dirty(Player, To) :-  % not used!
    bar(Player, BarCount), BarCount > 0,
    entry_point_length(Player, To, L),
    current_dice(Dice), member(L, Dice),
    move_from_bar(Player, To),
    use_die(L).


