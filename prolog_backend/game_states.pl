% Initial game state and other game states for testing

:- dynamic point/3, bar/2, off/2, current_dice/1.

% INITIAL BOARD SETUP
initial_state :-
    % Clear previous state
    retractall(point(_, _, _)),
    retractall(bar(_, _)),
    retractall(off(_, _)),

    % Black pieces (player -1)
    asserta(point(1, black, 2)),
    asserta(point(12, black, 5)),
    asserta(point(17, black, 3)),
    asserta(point(19, black, 5)),
    
    % White pieces (player 1)
    asserta(point(6, white, 5)),
    asserta(point(8, white, 3)),
    asserta(point(13, white, 5)),
    asserta(point(24, white, 2)),
    
    % Bar and off setup
    asserta(bar(white, 0)),
    asserta(bar(black, 0)),
    asserta(off(white, 0)),
    asserta(off(black, 0)).

can_bear_off_state :-
    retractall(point(_, _, _)),
    retractall(bar(_, _)),
    retractall(off(_, _)),

    % All checkers are positioned in the home board (points 1-6).
    asserta(point(5, white, 5)),
    asserta(point(4, white, 5)),
    asserta(point(3, white, 3)),
    asserta(point(1, white, 2)),

    % Black's checkers are positioned in their own home board
    asserta(point(20, black, 5)),
    asserta(point(21, black, 5)),
    asserta(point(22, black, 3)),
    asserta(point(24, black, 2)),

    % Ensure no players have checkers on the bar or off the board.
    asserta(bar(white, 0)),
    asserta(bar(black, 0)),
    asserta(off(white, 0)),
    asserta(off(black, 0)).

black_can_bar_state :-
    retractall(point(_, _, _)),
    retractall(bar(_, _)),
    retractall(off(_, _)),

    asserta(bar(black, 1)),          % One black checker is ON THE BAR.
    asserta(point(24, black, 14)),   % The other 14 are on the board.
    asserta(off(black, 0)),

    asserta(point(3, white, 2)),     % Point 3 is BLOCKED.
    asserta(point(5, white, 2)),     % Point 5 is BLOCKED.
    asserta(point(4, white, 1)),     % Point 4 is a BLOT (can be hit).

    asserta(point(8, white, 10)),    % The rest of white's checkers are out of the way.
    asserta(bar(white, 0)),
    asserta(off(white, 0)).

can_leave_bar_state :-
    % Clear previous state
    retractall(point(_, _, _)),
    retractall(bar(_, _)),
    retractall(off(_, _)),
    retractall(current_dice(_)),

    % Black owns all points in their home board (19 to 24) with 2+ pieces each
    asserta(point(19, black, 2)),
    asserta(point(20, black, 2)),
    asserta(point(21, black, 2)),
    asserta(point(22, black, 2)),
    asserta(point(23, black, 2)),
    asserta(point(24, black, 2)),

    % White has 2 pieces on the bar
    asserta(bar(white, 2)),

    % Black has nothing on the bar, and no pieces borne off
    asserta(bar(black, 0)),
    asserta(off(white, 0)),
    asserta(off(black, 0)).

can_leave_bar_state2 :-
    % Clear previous state
    retractall(point(_, _, _)),
    retractall(bar(_, _)),
    retractall(off(_, _)),
    retractall(current_dice(_)),

    % Black owns all points in their home board (19 to 24) with 2+ pieces each
    asserta(point(19, black, 1)),
    asserta(point(20, black, 1)),
    asserta(point(21, black, 1)),
    asserta(point(22, black, 1)),
    asserta(point(23, black, 1)),
    asserta(point(24, black, 1)),

    % White has 2 pieces on the bar
    asserta(bar(white, 2)),

    % Black has nothing on the bar, and no pieces borne off
    asserta(bar(black, 0)),
    asserta(off(white, 0)),
    asserta(off(black, 0)).