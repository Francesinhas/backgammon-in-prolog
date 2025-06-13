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
:- discontiguous can_bear_off_state/0.
:- discontiguous black_can_bar_state/0.
:- discontiguous validate_and_get_bear_off_die/3.
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

validate_and_get_bear_off_die(Player, Point, UsedDie) :-
    bear_off(Player, Point), % Checks if the player is in a bear-off state and a checker exists on the point.
    current_dice(DiceList),
    (Player = white -> Dist is Point ; Dist is 25 - Point),
    (
        % Case 1: An exact die is available.
        member(Dist, DiceList) ->
        UsedDie = Dist
    ;
        % Case 2: Overshoot is allowed if no checkers are on higher points.
        % Check that there are no checkers on points further from the goal.
        \+ (point(HigherPoint, Player, C), C > 0, (Player = white -> HigherPoint > Point ; HigherPoint < Point)),
        % Find all available dice that are greater than the required distance.
        findall(D, (member(D, DiceList), D > Dist), BiggerDice),
        BiggerDice \= [],
        % Use the smallest possible die for the move.
        sort(BiggerDice, [UsedDie|_]) % Sorts ascending and unifies UsedDie with the head.
    ).

bear_off_with_dice(Player, Point) :-
    % 1. Check if the player is in a state to bear off from this point
    bear_off(Player, Point),

    % 2. Check if the required die is available in the current roll
    current_dice(DiceList),
    (Player = white -> Dist is Point ; Dist is 25 - Point),
    member(Dist, DiceList),

    % 3. Execute the move: remove the checker from the point and update the 'off' count
    retract(point(Point, Player, Count)),
    NewCount is Count - 1,
    (NewCount > 0 -> asserta(point(Point, Player, NewCount)) ; true). % Re-assert the point only if checkers remain

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