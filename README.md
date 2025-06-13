# Backgammon in Prolog
A classic implementation of the game Backgammon, written in Prolog. This project focuses on game logic, player interactions, and board representation using declarative programming principles.

## Getting started
### 1. **Clone the repository**
	git clone https://github.com/Francesinhas/backgammon-in-prolog.git

### 2. **Install Prolog**

- For Windows: Download and install [SWI-Prolog](https://www.swi-prolog.org/download/stable)
- For macOS: `brew install swi-prolog`
- For Linux (Debian/Ubuntu): `sudo apt-get install swi-prolog`
### 3. **Set Up Python Environment**
	pip install pyswip
	pip install pygame
### 4. **Run the game**
	python app.py
### 5. (Optional - for development purposes) **VS Code Setup**
- Install the "VSC-Prolog" extension by "arthwang" for VS Code. This extension provides syntax highlighting and query support.

## Project Structure
### Prolog backend

- We'll start by defining the board representation and basic predicates in `game_rules.pl`.
- Then, we'll handle state transitions in `state_manager.pl`.
- Finally, we'll create a simple AI in `ai.pl`.

### Python interface

TODO

  

## Workflow

1. We'll define the board using the Tesauro encoding. We'll represent the board as 24 points, each with a color and a number of checkers. Also, we have the bar and off for each player.
  

![image](/resources/backgammon_board.png)


## Plan
- [ ] Prolog logic
	- [ ] implement project basic structure
	- [ ] check logic in game rules
	- [ ] check updated logic in state manager
	- [ ] implement AI
- [ ] link .pl files together #test
- [ ] connect to python #important
	- [ ] define prolog interface
	- [ ] start implementing python gui

### Improvements
- possible issues:
	- [ ] valid_move - target computation 
		- why compute target if To is the target? #q 
	- [ ] state_manager 
		- [ ] doesnt check if moves are valid, should it? #q 
		- [ ] move_from_bar calls apply_move(Player, bar, To). bar doesn't look like a valid argument
			- [ ] implement the logic for placing the piece directly there or in another rule
	- [ ] 
- relevant logic:
	- [ ] checking the dice numbers when figuring if a move or bearing can be made (not in game_rules yet)
	- [ ] 
- nice to have:
	- [ ] state_manager:
		- [ ] don t assert back a number of poins after a move if it drops under 0

### GUI communication with prolog
→ init_game()
→ show_board()
→ while not winner:
    → player_turn()
        → roll_dice → get_input → validate/apply → update board
    → check_winner
    → ai_turn()
        → choose_move → apply → update board
    → check_winner
→ show_winner()

### Tests
- [X] check if capturing a piece actually puts it on the bar
	- [X] check if the bar state is sent to state_to_list
- [X] check perfom move from bar