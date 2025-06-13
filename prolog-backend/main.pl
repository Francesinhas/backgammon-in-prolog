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


available_moves(Player, Moves) :-   % todo also include moving from bar and bearing off
    findall(move(From, To), choose_move_with_dice(Player, move(From, To)), RawMoves),
    list_to_set(RawMoves, Moves).  % avoid duplicates

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
    bear_off(Player, Point),        % todo - replace check with bear_off_with_dice and then with bear_off_with_dice_real
    bear_off_piece(Player, Point),
    bear_off_length(Player, Point, L),
    use_die(L).



perform_move_from_bar_dirty(Player, To) :-  % not used!
    bar(Player, BarCount), BarCount > 0,
    entry_point_length(Player, To, L),
    current_dice(Dice), member(L, Dice),
    move_from_bar(Player, To),
    use_die(L).


% helper predicates:

move_length(Player, From, To, L) :-
    (Player = white -> L is From - To ; L is To - From).

bear_off_length(white, Point, L) :- L is Point.
bear_off_length(black, Point, L) :- L is 25 - Point.
