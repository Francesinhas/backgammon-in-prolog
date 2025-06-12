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

![image](/backgammon_board.png)