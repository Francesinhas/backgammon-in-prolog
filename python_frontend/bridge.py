from pyswip import Prolog, Variable
import re

prolog = Prolog()
prolog.consult("main.pl")

def get_current_board_state():
    state_query = list(prolog.query("state_to_list(StateList).", maxresult=1))
    board_state = state_query[0]["StateList"] if state_query else None

    white_points = [0] * 24
    black_points = [0] * 24
    bar_white = bar_black = off_white = off_black = 0

    for i, term in enumerate(board_state):
        try:
            # Match structure like -(-(24, white), 2)
            match = re.match(r"-\(-\((\d+),\s*(white|black)\),\s*(\d+)\)", term)
            if match:
                pos = int(match.group(1))
                color = match.group(2)
                count = int(match.group(3))
                if color == "white":
                    white_points[pos-1] = count
                else:
                    black_points[pos-1] = count
                continue

            # Match structure bar/off terms like -(white, 0)
            match = re.match(r"-\((white|black),\s*(\d+)\)", term)
            if match:
                color = match.group(1)
                count = int(match.group(2))

                # Use index: first two are bar, last two are off
                if i < len(board_state) // 2:
                    if color == "white":
                        bar_white = count
                    else:
                        bar_black = count
                else:
                    if color == "white":
                        off_white = count
                    else:
                        off_black = count
                continue

            print(f"Unrecognized term: {term}")
        except Exception as e:
            print(f"Error parsing term: {term} -> {e}")

    return [
        white_points,
        black_points,
        bar_white,
        bar_black,
        off_white,
        off_black
    ]
def reset_board():
    # Initialize game state
    initial_query = bool(list(prolog.query("initial_state.")))

    return get_current_board_state()

def get_dice():
    dice_query = list(prolog.query("dice_roll(Dice).", maxresult=1))
    dice = dice_query[0]["Dice"] if dice_query else None
    dice = dice + [dice[0], dice[0]] if dice[0] == dice[1] else dice

    return dice


dice = get_dice()
reset_board()
parsed_state = get_current_board_state()

# Display result
for i in parsed_state:
    print(f"{i}")
