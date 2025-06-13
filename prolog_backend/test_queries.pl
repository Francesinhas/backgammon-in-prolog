:- include('game_rules').

:- discontiguous move_from_bar/2.
:- discontiguous perform_move/3.
:- discontiguous choose_move/3.
:- discontiguous perform_move_from_bar/2.



% game_rules testing
initial_state.
?- point(12, white, Count).
point(3, Player, Count).

% Create scenario where white can bear off
retractall(point(_,_,_)), asserta(point(1, white, 2)), asserta(point(5, white, 1)).
can_bear_off(white).

% Add a checker outside home board -> cannot bear off
asserta(point(7, white, 1)).
can_bear_off(white).

test_valid_moves :-
    initial_state,
    % Should succeed (white can move from 19 to 16)
    valid_move(white, 24, 21),
    % Should fail (wrong direction)
    \+ valid_move(white, 13, 14),
    % Should fail (black's piece)
    \+ valid_move(white, 13, 12),
    % Should be true, to place on black's one piece
    (asserta(point(11, black, 1)),
    valid_move(white, 13, 11)),
    write('All tests passed').

% Test capturing a piece
test_capture_and_move_from_bar:-
    initial_state,
    % Place a piece alone on position 11
    asserta(point(11, black, 1)), valid_move(white,13,11),
    % Capture is performed from pos 13 to pos 11
    perform_move(white,13,11),
    % Should be 1 on the black bar
    bar(black,1),
    % Should fail because no piece on position 11
    \+ point(11,black,X),
    move_from_bar(black,22),
    bar(black,0).

test-perform-form-bar:-
    initial_state,
    asserta(bar(white,1)),
    asserta(current_dice([2])),
    perform_move_from_bar(white,23).

test :-
    initial_state,
    valid_move(white, 17, 12),  % Should succeed
    valid_move(white, 1, 30),   % Should fail (invalid position)
    dice_roll(Dice),
    write('Dice roll: '), writeln(Dice),
    choose_move(white, [3,2], Move),
    write('AI move: '), writeln(Move).