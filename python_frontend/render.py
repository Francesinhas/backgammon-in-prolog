# render.py
import pygame
from constants import *

# --------------------------------------------------------------------------- #
#  Geometry helpers                                                           #
# --------------------------------------------------------------------------- #
def get_point_x(idx: int) -> int:
    idx -= 1
    if idx < 6:      return RIGHT_ZONE_START + (5  - idx) * POINT_WIDTH
    if idx < 12:     return LEFT_ZONE_START  + (11 - idx) * POINT_WIDTH
    if idx < 18:     return LEFT_ZONE_START  + (idx - 12) * POINT_WIDTH
    return RIGHT_ZONE_START + (idx - 18) * POINT_WIDTH


def get_triangle_rect(idx: int) -> pygame.Rect:
    x = get_point_x(idx)
    y = 0 if idx > 12 else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)


def get_bar_rect(is_white: bool) -> pygame.Rect:
    x = BAR_START_X + BAR_WIDTH // 2 - POINT_WIDTH // 2
    y = 0 if is_white else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)


def get_off_rect(is_white: bool) -> pygame.Rect:
    x = RIGHT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + 2 * CHECKER_RADIUS
    x -= POINT_WIDTH // 2
    y = 0 if is_white else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)


# --------------------------------------------------------------------------- #
#  Drawing helpers                                                            #
# --------------------------------------------------------------------------- #
def draw_board(screen):
    screen.fill(BOARD_COLOR)
    for i in range(1, 25):
        x = get_point_x(i)
        y = 0 if i > 12 else WINDOW_HEIGHT
        direction = 1 if i > 12 else -1
        colour = (139, 69, 19) if i % 2 else (160, 82, 45)
        pygame.draw.polygon(
            screen, colour,
            [(x, y), (x + POINT_WIDTH, y), (x + POINT_WIDTH // 2, y + direction * POINT_HEIGHT)]
        )
    pygame.draw.rect(screen, (105, 105, 105), (BAR_START_X, 0, BAR_WIDTH, WINDOW_HEIGHT))


def draw_checkers(screen, white, black):
    for i in range(1, 25):
        x = get_point_x(i) + POINT_WIDTH // 2
        if i <= 12:
            wy = WINDOW_HEIGHT - CHECKER_RADIUS
            by = wy - 2 * CHECKER_RADIUS * white[i]
            for j in range(white[i]):
                pygame.draw.circle(screen, WHITE, (x, wy - j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
            for j in range(black[i]):
                pygame.draw.circle(screen, BLACK, (x, by - j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
        else:
            wy = CHECKER_RADIUS
            by = wy + 2 * CHECKER_RADIUS * white[i]
            for j in range(white[i]):
                pygame.draw.circle(screen, WHITE, (x, wy + j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
            for j in range(black[i]):
                pygame.draw.circle(screen, BLACK, (x, by + j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)


def draw_bar_and_off(screen, bar_w, bar_b, off_w, off_b):
    x = BAR_START_X + BAR_WIDTH // 2
    for j in range(bar_w):
        pygame.draw.circle(screen, WHITE, (x, CHECKER_RADIUS + j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
    for j in range(bar_b):
        pygame.draw.circle(screen, BLACK, (x, WINDOW_HEIGHT - CHECKER_RADIUS - j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)

    x_white = RIGHT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + 2 * CHECKER_RADIUS
    for j in range(off_w):
        pygame.draw.circle(screen, WHITE, (x_white, OFF_CHECKER_RADIUS + j * 2 * OFF_CHECKER_RADIUS), OFF_CHECKER_RADIUS)
    for j in range(off_b):
        pygame.draw.circle(screen, BLACK,
                           (x_white, WINDOW_HEIGHT - OFF_CHECKER_RADIUS - j * 2 * OFF_CHECKER_RADIUS),
                           OFF_CHECKER_RADIUS)


def draw_dice(screen, dice):
    font = pygame.font.SysFont(None, DICE_FONT_SIZE)
    total_w = len(dice) * (DICE_SIZE + DICE_GAP) - DICE_GAP
    x0 = (WINDOW_WIDTH - total_w) // 2
    y  = (WINDOW_HEIGHT - DICE_SIZE) // 2
    for i, v in enumerate(dice):
        x = x0 + i * (DICE_SIZE + DICE_GAP)
        pygame.draw.rect(screen, (245, 245, 245), (x, y, DICE_SIZE, DICE_SIZE))
        pygame.draw.rect(screen, (0, 0, 0),     (x, y, DICE_SIZE, DICE_SIZE), 2)
        txt = font.render(str(v), True, (0, 0, 0))
        screen.blit(txt, txt.get_rect(center=(x + DICE_SIZE // 2, y + DICE_SIZE // 2)))


def draw_triangle_buttons(screen, fr, to):
    def shade(rect, col):
        s = pygame.Surface((POINT_WIDTH, POINT_HEIGHT), pygame.SRCALPHA)
        s.fill(col)
        screen.blit(s, rect.topleft)

    for i in range(1, 25):
        rect = get_triangle_rect(i)
        colour = (255, 255, 0, 80) if fr == i else (0, 0, 255, 80) if to == i else (0, 0, 0, 0)
        shade(rect, colour)

    for idx, white in [(BAR_WHITE_IDX, True), (BAR_BLACK_IDX, False)]:
        colour = (255, 255, 0, 80) if fr == idx else (0, 0, 255, 80) if to == idx else (0, 0, 0, 0)
        shade(get_bar_rect(white), colour)

    for idx, white in [(OFF_WHITE_IDX, True), (OFF_BLACK_IDX, False)]:
        colour = (255, 255, 0, 80) if fr == idx else (0, 0, 255, 80) if to == idx else (0, 0, 0, 0)
        shade(get_off_rect(white), colour)


def draw_popup(screen, fr, to):
    pygame.draw.rect(screen, (240, 240, 240), popup_rect)
    pygame.draw.rect(screen, (0, 0, 0), popup_rect, 2)
    font = pygame.font.SysFont(None, 24)
    msg  = font.render(f"Confirm move from {fr} to {to}?", True, (0, 0, 0))
    screen.blit(msg, msg.get_rect(center=(popup_rect.centerx, popup_rect.y + 30)))

    pygame.draw.rect(screen, (200, 200, 200), confirm_button_rect)
    pygame.draw.rect(screen, (0, 0, 0),     confirm_button_rect, 2)
    ok = font.render("Confirm", True, (0, 0, 0))
    screen.blit(ok, ok.get_rect(center=confirm_button_rect.center))


def draw_invalid_move_popup(screen):
    pop = pygame.Rect(WINDOW_WIDTH // 2 - 150, WINDOW_HEIGHT // 2 - 30, 300, 60)
    pygame.draw.rect(screen, (255, 200, 200), pop)
    pygame.draw.rect(screen, (200,   0,   0), pop, 2)
    font = pygame.font.SysFont(None, 28)
    txt  = font.render("Invalid move!", True, (200, 0, 0))
    screen.blit(txt, txt.get_rect(center=pop.center))


# --------------------------------------------------------------------------- #
#  NEW : “no moves” modal popup                                               #
# --------------------------------------------------------------------------- #
def draw_no_moves_popup(screen, player: str):
    pygame.draw.rect(screen, (240, 240, 240), no_moves_popup_rect)
    pygame.draw.rect(screen, (0, 0, 0),       no_moves_popup_rect, 2)

    font = pygame.font.SysFont(None, 24)
    msg  = "No possible moves!" if player == "white" else "AI has no possible moves!"
    txt  = font.render(msg, True, (0, 0, 0))
    screen.blit(txt, txt.get_rect(center=(no_moves_popup_rect.centerx, no_moves_popup_rect.y + 30)))

    pygame.draw.rect(screen, (200, 200, 200), no_moves_button_rect)
    pygame.draw.rect(screen, (0, 0, 0),       no_moves_button_rect, 2)
    label = "Pass Turn" if player == "white" else "Continue"
    btn   = font.render(label, True, (0, 0, 0))
    screen.blit(btn, btn.get_rect(center=no_moves_button_rect.center))
