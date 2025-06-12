# Backgammon in Prolog


## Set up
### 1. **Install Prolog**

- For Windows: Download and install [SWI-Prolog](https://www.swi-prolog.org/download/stable)
- For macOS: `brew install swi-prolog`
- For Linux (Debian/Ubuntu): `sudo apt-get install swi-prolog`

### 2. **VS Code Setup**

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
	- [ ] 
- relevant logic:
	- [ ] checking the dice numbers when figuring if a move or bearing can be made (not in game_rules yet)
	- [ ] 
- nice to have:
	- [ ] 