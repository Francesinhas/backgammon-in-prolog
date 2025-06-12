% State transitions and game operations
% Inspired by: https://github.com/rlg2161/Backgammon

:- include('game_rules').

% APPLY MOVE
apply_move(Player, From, To) :-
    % Remove piece from source
    retract(point(From, Player, CountFrom)),
    NewCountFrom is CountFrom - 1,
    asserta(point(From, Player, NewCountFrom)),
    
    % Handle hit opponent if exists
    (   point(To, Opponent, OppCount), Opponent \= Player, OppCount > 0
    ->  retract(point(To, Opponent, OppCount)),
        update_bar(Opponent, 1),
        asserta(point(To, Player, 1))
    ;   (   point(To, Player, CountTo)
        ->  retract(point(To, Player, CountTo)),
            NewCountTo is CountTo + 1,
            asserta(point(To, Player, NewCountTo))
        ;   asserta(point(To, Player, 1))
        )
    ).

% MOVE FROM BAR
move_from_bar(Player, To) :-
    bar(Player, BarCount), BarCount > 0,
    retract(bar(Player, BarCount)),
    NewBarCount is BarCount - 1,
    asserta(bar(Player, NewBarCount)),
    apply_move(Player, bar, To).

% BEAR OFF PIECE
bear_off_piece(Player, Point) :-
    bear_off(Player, Point),
    retract(point(Point, Player, Count)),
    NewCount is Count - 1,
    asserta(point(Point, Player, NewCount)),
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