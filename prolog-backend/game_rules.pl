% Backgammon rules implementation
% Based on Tesauro's encoding scheme from gym-backgammon
% Reference: https://github.com/dellalibera/gym-backgammon

:- dynamic point/3, bar/2, off/2.

bos(da).    %#q what is this?

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

% MOVE VALIDATION
valid_move(Player, From, To) :-
    % Cannot move opponent's pieces
    point(From, Player, Count), Count > 0,
    
    % Calculate target position based on player direction
    (Player = white -> Target is From - To ; Target is To - From), %#q is the target computation useful or even
    between(1, 24, Target),
    
    % Check target point availability
    (   point(Target, Player, _) -> true  % Can stack on own pieces
    ;   point(Target, Opponent, OppCount), 
        Opponent \= Player, 
        OppCount =< 1 -> true  % Can hit if 1 or fewer opponents    %#q is the return true really necessary?
    ;   \+ point(Target, _, _)  % Empty point
    ),
    
    % Special case: Cannot move if pieces on bar
    bar(Player, BarCount), BarCount =:= 0.  %#q may move to the top and replace with bar(Player, 0).

% BEARING OFF
can_bear_off(Player) :-
    % All pieces in home board (1-6 for white, 19-24 for black)
    forall(point(Point, Player, _), 
           (Player = white -> Point =< 6 ; Point >= 19)).

bear_off(Player, Point) :-
    can_bear_off(Player),
    point(Point, Player, Count), Count > 0,
    (Player = white -> Point =< 6 ; Point >= 19).

% WIN CONDITION
winner(Player) :- off(Player, 15).

% DICE ROLL
dice_roll([D1, D2]) :-
    random_between(1, 6, D1),
    random_between(1, 6, D2).