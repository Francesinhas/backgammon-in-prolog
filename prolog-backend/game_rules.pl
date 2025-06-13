% Backgammon rules implementation

% ATTEMPT TO USE MODULES to prevent discontigous errors
% :- module(game_rules, [
%     initial_state/0,
%     valid_move/3,
%     valid_move_with_dice/3,
%     can_bear_off/1,
%     bear_off/2,
%     winner/1,
%     dice_roll/1,
%     current_dice/1
% ]).

:- discontiguous initial_state/0.
:- discontiguous valid_move/3.
:- discontiguous valid_move_with_dice/3.
:- discontiguous can_bear_off/1.
:- discontiguous bear_off/2.
:- discontiguous winner/1.
:- discontiguous dice_roll/1.
:- discontiguous bear_off_with_dice/2.
:- discontiguous bear_off_with_dice_real/2.
:- discontiguous move_length/4.
:- discontiguous entry_point_length/3.
:- discontiguous bear_off_length/3.

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

% MOVE VALIDATION
valid_move(Player, From, To) :-
    point(From, Player, Count), Count > 0,  % Own piece exists
    % Only check direction if From and To are both instantiated,
    (   ground([From, To])
    ->  (Player = white -> To < From ; To > From)
    ;   true  % Defer if not yet known
    ),
    between(1, 24, To),  % Valid board position
    bar(Player, 0),  % No pieces on bar
    (   point(To, Player, _)        % Can land on own pieces
    ;   point(To, Opponent, OppCount), % Can hit single opponent
        OppCount =< 1,     
        Opponent \= Player
    ;   \+ point(To, _, _)           % Can land on empty point
    ).

% MOVE VALIDATION WITH DICE CHECK
valid_move_with_dice(Player, From, To) :-
    valid_move(Player, From, To),
    current_dice(DiceList),
    (   ground([From, To])
    ->  (Player = white -> Diff is From - To ; Diff is To - From),
        member(Diff, DiceList)
    ;   fail  % Avoid unsafe evaluation
    ).

% BEARING OFF
can_bear_off(Player) :-
    % Must have no pieces on bar
    bar(Player,0),
    % All pieces in home board (1-6 for white, 19-24 for black)
    \+ (point(Point, Player, Count), 
        Count > 0,
        (Player = white -> (Point < 1 ; Point > 6) ; (Point < 19 ; Point > 24))
    ).

bear_off(Player, Point) :-
    can_bear_off(Player),
    point(Point, Player, Count),
    Count > 0,
    (Player = white -> between(1, 6, Point) ; between(19, 24, Point)).

bear_off_with_dice(Player, Point) :-
    bear_off(Player, Point),
    current_dice(DiceList),
    (Player = white -> Dist is Point ; Dist is 25 - Point),
    member(Dist, DiceList).

% better logic, but not tested
bear_off_with_dice_real(Player, Point) :-
    bear_off(Player, Point),
    current_dice(DiceList),
    (Player = white -> Dist is Point ; Dist is 25 - Point),
    (   member(Dist, DiceList)
    ;   % Allow overshoot if no checkers on higher points
        findall(P, (point(P, Player, C), C > 0, 
                    (Player = white -> P > Point ; P < Point)), Higher),
        Higher == [],
        member(Bigger, DiceList),
        Bigger > Dist
    ).

% WIN CONDITION
winner(Player) :-
    off(Player, 15).

% DICE ROLL
dice_roll(Dice) :-
    random_between(1, 6, D1),
    random_between(1, 6, D2),
    (D1 =:= D2 ->
        Dice = [D1, D1, D1, D1]  % Handle doubles
    ;
        Dice = [D1, D2]
    ),
    retractall(current_dice(_)),
    assertz(current_dice(Dice)).


% helper predicates:

move_length(Player, From, To, L) :-
    (Player = white -> L is From - To ; L is To - From).

entry_point_length(white, To, L) :-
    ( ground([To]) -> L is 25 - To ; fail ).
entry_point_length(black, To, L) :-
    ( ground([To]) -> L is To ; fail ).

% TODO Check this later
bear_off_length(white, Point, L) :- L is Point.
bear_off_length(black, Point, L) :- L is 25 - Point.