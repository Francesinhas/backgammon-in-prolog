% State transitions and game operations

:- discontiguous use_die/1.
:- discontiguous land_on/2.
:- discontiguous apply_move/3.
:- discontiguous move_from_bar/2.
:- discontiguous bear_off_piece/2.
:- discontiguous update_bar/2.
:- discontiguous update_off/2.
:- discontiguous state_to_list/1.
:- discontiguous move_from_bar_with_dice/2.

:- include('game_rules').

% CONSUME USED DIE
use_die(Length) :-
    current_dice(Dice),
    select(Length, Dice, NewDice),
    retract(current_dice(Dice)),
    assertz(current_dice(NewDice)).

% HANDLE HIT IF OPPONENT EXISTS
land_on(Player, To) :-
    (   point(To, Opponent, 1), Opponent \= Player
    ->  retract(point(To, Opponent, 1)),
        update_bar(Opponent, 1),
        asserta(point(To, Player, 1))
    ;   point(To, Player, CountTo)
    ->  retract(point(To, Player, CountTo)),
        NewCountTo is CountTo + 1,
        asserta(point(To, Player, NewCountTo))
    ;   \+ point(To, _, _)
    ->  asserta(point(To, Player, 1))
    ).

% APPLY MOVE
apply_move(Player, From, To) :-
    % Remove piece from source
    retract(point(From, Player, CountFrom)),
    NewCountFrom is CountFrom - 1,
    (NewCountFrom > 0 -> asserta(point(From, Player, NewCountFrom)) ; true),
    land_on(Player, To).

% MOVE FROM BAR
move_from_bar_with_dice(Player, To) :-
    bar(Player, BarCount), BarCount > 0,
    entry_point_length(Player, To, L),
    current_dice(Dice), member(L, Dice),
    retract(bar(Player, BarCount)),
    NewBarCount is BarCount - 1,
    asserta(bar(Player, NewBarCount)),
    land_on(Player, To).

move_from_bar(Player, To) :-
    bar(Player, BarCount), BarCount > 0,
    retract(bar(Player, BarCount)),
    NewBarCount is BarCount - 1,
    asserta(bar(Player, NewBarCount)),
    land_on(Player, To).

% BEAR OFF PIECE
bear_off_piece(Player, Point) :-
    retract(point(Point, Player, Count)),
    NewCount is Count - 1,
    (NewCount > 0 ->
        asserta(point(Point, Player, NewCount)) % Re-assert the point only if checkers remain.
    ;
        true % Do not assert the point if it is now empty.
    ),
    update_off(Player, 1).

% UPDATE BAR COUNTER
update_bar(Player, N) :-
    bar(Player, Current),
    retract(bar(Player, Current)),
    New is Current + N,
    asserta(bar(Player, New)).

% UPDATE OFF COUNTER
update_off(Player, N) :-
    off(Player, Current),
    retract(off(Player, Current)),
    New is Current + N,
    asserta(off(Player, New)).

% GAME STATE TO LIST (for Python interface)
state_to_list(StateList) :-
    findall(Point-Color-Count, point(Point, Color, Count), Points),
    findall(Color-BarCount, bar(Color, BarCount), Bar),
    findall(Color-OffCount, off(Color, OffCount), Off),
    append([Points, Bar, Off], StateList).
