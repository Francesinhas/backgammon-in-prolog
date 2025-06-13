# constants.py
import pygame

# --------------------------------------------------------------------------- #
#  Window / board geometry                                                    #
# --------------------------------------------------------------------------- #
WINDOW_WIDTH  = 900
WINDOW_HEIGHT = 600
BOARD_COLOR   = (222, 184, 135)

POINT_WIDTH   = 60
POINT_HEIGHT  = 250
NUM_TRIANGLES_PER_SIDE = 6

CHECKER_RADIUS     = 20
OFF_CHECKER_RADIUS = 14
BAR_WIDTH          = CHECKER_RADIUS * 2

WHITE = (255, 255, 255)
BLACK = (0,   0,   0)

# Dice
DICE_SIZE      = 50
DICE_GAP       = 10
DICE_FONT_SIZE = 28

# Logical indexes
BAR_WHITE_IDX = 25
BAR_BLACK_IDX = 26
OFF_WHITE_IDX = 27
OFF_BLACK_IDX = 28

# Pre-computed geometry
TOTAL_PLAY_WIDTH = 2 * NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + BAR_WIDTH
MARGIN_X         = (WINDOW_WIDTH - TOTAL_PLAY_WIDTH) // 2
LEFT_ZONE_START  = MARGIN_X
BAR_START_X      = LEFT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH
RIGHT_ZONE_START = BAR_START_X  + BAR_WIDTH

# --------------------------------------------------------------------------- #
#  Interaction / UI state (initial values)                                    #
# --------------------------------------------------------------------------- #
selected_from = None
selected_to   = None

# Move-confirmation popup
show_popup   = False
popup_rect   = pygame.Rect(WINDOW_WIDTH // 2 - 200, WINDOW_HEIGHT // 2 - 50, 400, 100)
confirm_button_rect = pygame.Rect(popup_rect.centerx - 60, popup_rect.y + 50, 120, 30)

# Dice roll button
roll_button_rect = pygame.Rect(WINDOW_WIDTH // 2 - 75, 20, 150, 40)

# Invalid-move transient popup
show_invalid_move_popup = False
invalid_popup_timer     = 0

# Dice-rolled flag (per turn)
has_rolled_dice = False

# AI button
ai_move_button_rect = pygame.Rect(WINDOW_WIDTH // 2 - 75, 70, 150, 40)

# “No moves” popup
no_moves_popup_rect  = pygame.Rect(WINDOW_WIDTH // 2 - 200, WINDOW_HEIGHT // 2 - 60, 400, 120)
no_moves_button_rect = pygame.Rect(no_moves_popup_rect.centerx - 60, no_moves_popup_rect.y + 60, 120, 30)

# Winner state
winner_player = None  # "white" or "black"
