:- include('game_rules').
% :- include('state_manager.pl').
% :- include('ai.pl').

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

test :-
    initial_state,
    valid_move(white, 17, 12),  % Should succeed
    valid_move(white, 1, 30),   % Should fail (invalid position)
    dice_roll(Dice),
    write('Dice roll: '), writeln(Dice),
    choose_move(white, [3,2], Move),
    write('AI move: '), writeln(Move).