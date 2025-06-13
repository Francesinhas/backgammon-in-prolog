# constants.py
import pygame

# --- Constants ---
WINDOW_WIDTH = 900
WINDOW_HEIGHT = 600
BOARD_COLOR = (222, 184, 135)
POINT_WIDTH = 60
POINT_HEIGHT = 250
CHECKER_RADIUS = 20
OFF_CHECKER_RADIUS = 14
BAR_WIDTH = CHECKER_RADIUS * 2
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
NUM_TRIANGLES_PER_SIDE = 6

DICE_SIZE = 50
DICE_GAP = 10
DICE_FONT_SIZE = 28

BAR_WHITE_IDX = 25
BAR_BLACK_IDX = 26
OFF_WHITE_IDX = 27
OFF_BLACK_IDX = 28

TOTAL_PLAY_WIDTH = 2 * NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + BAR_WIDTH
MARGIN_X = (WINDOW_WIDTH - TOTAL_PLAY_WIDTH) // 2
LEFT_ZONE_START = MARGIN_X
BAR_START_X = LEFT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH
RIGHT_ZONE_START = BAR_START_X + BAR_WIDTH

selected_from = None
selected_to = None
show_popup = False
popup_rect = pygame.Rect(WINDOW_WIDTH // 2 - 200, WINDOW_HEIGHT // 2 - 50, 400, 100)
confirm_button_rect = pygame.Rect(popup_rect.centerx - 60, popup_rect.y + 50, 120, 30)
roll_button_rect = pygame.Rect(WINDOW_WIDTH // 2 - 75, 20, 150, 40)
show_invalid_move_popup = False
invalid_popup_timer = 0
has_rolled_dice = False

ai_move_button_rect = pygame.Rect(WINDOW_WIDTH // 2 - 75, 70, 150, 40)
