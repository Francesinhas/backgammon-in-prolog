# draw_utils.py
import pygame
from constants import *

def get_point_x(index):
    index -= 1
    if index < 6:
        return RIGHT_ZONE_START + (5 - index) * POINT_WIDTH
    elif index < 12:
        return LEFT_ZONE_START + (11 - index) * POINT_WIDTH
    elif index < 18:
        return LEFT_ZONE_START + (index - 12) * POINT_WIDTH
    else:
        return RIGHT_ZONE_START + (index - 18) * POINT_WIDTH

def get_triangle_rect(index):
    x = get_point_x(index)
    y = 0 if index > 12 else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)

def get_bar_rect(is_white):
    x = BAR_START_X + BAR_WIDTH // 2 - POINT_WIDTH // 2
    y = 0 if is_white else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)

def get_off_rect(is_white):
    x = RIGHT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + 2 * CHECKER_RADIUS
    x -= POINT_WIDTH // 2
    y = 0 if is_white else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)

def draw_board(screen):
    screen.fill(BOARD_COLOR)
    for i in range(1, 25):
        x = get_point_x(i)
        y = 0 if i > 12 else WINDOW_HEIGHT
        direction = 1 if i > 12 else -1
        color = (139, 69, 19) if i % 2 == 1 else (160, 82, 45)
        pygame.draw.polygon(screen, color, [
            (x, y),
            (x + POINT_WIDTH, y),
            (x + POINT_WIDTH // 2, y + direction * POINT_HEIGHT)
        ])
    pygame.draw.rect(screen, (105, 105, 105), (BAR_START_X, 0, BAR_WIDTH, WINDOW_HEIGHT))

def draw_checkers(screen, white_checkers, black_checkers):
    for i in range(1, 25):
        x = get_point_x(i) + POINT_WIDTH // 2
        if i <= 12:
            wy = WINDOW_HEIGHT - CHECKER_RADIUS
            by = wy - CHECKER_RADIUS * 2 * white_checkers[i]
            for j in range(white_checkers[i]):
                pygame.draw.circle(screen, WHITE, (x, wy - j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
            for j in range(black_checkers[i]):
                pygame.draw.circle(screen, BLACK, (x, by - j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
        else:
            wy = CHECKER_RADIUS
            by = wy + CHECKER_RADIUS * 2 * white_checkers[i]
            for j in range(white_checkers[i]):
                pygame.draw.circle(screen, WHITE, (x, wy + j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)
            for j in range(black_checkers[i]):
                pygame.draw.circle(screen, BLACK, (x, by + j * 2 * CHECKER_RADIUS), CHECKER_RADIUS)

def draw_bar_and_off(screen, bar_white, bar_black, off_white, off_black):
    x = BAR_START_X + BAR_WIDTH // 2
    for j in range(bar_white):
        y = CHECKER_RADIUS + j * 2 * CHECKER_RADIUS
        pygame.draw.circle(screen, WHITE, (x, y), CHECKER_RADIUS)
    for j in range(bar_black):
        y = WINDOW_HEIGHT - CHECKER_RADIUS - j * 2 * CHECKER_RADIUS
        pygame.draw.circle(screen, BLACK, (x, y), CHECKER_RADIUS)

    x_white = RIGHT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + 2 * CHECKER_RADIUS
    for j in range(off_white):
        y = OFF_CHECKER_RADIUS + j * 2 * OFF_CHECKER_RADIUS
        pygame.draw.circle(screen, WHITE, (x_white, y), OFF_CHECKER_RADIUS)
    for j in range(off_black):
        y = WINDOW_HEIGHT - OFF_CHECKER_RADIUS - j * 2 * OFF_CHECKER_RADIUS
        pygame.draw.circle(screen, BLACK, (x_white, y), OFF_CHECKER_RADIUS)

def draw_dice(screen, dice_values):
    font = pygame.font.SysFont(None, DICE_FONT_SIZE)
    total_width = len(dice_values) * (DICE_SIZE + DICE_GAP) - DICE_GAP
    x_start = (WINDOW_WIDTH - total_width) // 2
    y = (WINDOW_HEIGHT - DICE_SIZE) // 2
    for i, value in enumerate(dice_values):
        x = x_start + i * (DICE_SIZE + DICE_GAP)
        pygame.draw.rect(screen, (245, 245, 245), (x, y, DICE_SIZE, DICE_SIZE))
        pygame.draw.rect(screen, (0, 0, 0), (x, y, DICE_SIZE, DICE_SIZE), 2)
        text = font.render(str(value), True, (0, 0, 0))
        text_rect = text.get_rect(center=(x + DICE_SIZE // 2, y + DICE_SIZE // 2))
        screen.blit(text, text_rect)

def draw_triangle_buttons(screen, selected_from, selected_to):
    def draw_rect(rect, color):
        s = pygame.Surface((POINT_WIDTH, POINT_HEIGHT), pygame.SRCALPHA)
        s.fill(color)
        screen.blit(s, (rect.x, rect.y))

    for i in range(1, 25):
        rect = get_triangle_rect(i)
        color = (255, 255, 0, 80) if selected_from == i else (0, 0, 255, 80) if selected_to == i else (0, 0, 0, 0)
        draw_rect(rect, color)

    for idx, is_white in [(BAR_WHITE_IDX, True), (BAR_BLACK_IDX, False)]:
        rect = get_bar_rect(is_white)
        color = (255, 255, 0, 80) if selected_from == idx else (0, 0, 255, 80) if selected_to == idx else (0, 0, 0, 0)
        draw_rect(rect, color)

    for idx, is_white in [(OFF_WHITE_IDX, True), (OFF_BLACK_IDX, False)]:
        rect = get_off_rect(is_white)
        color = (255, 255, 0, 80) if selected_from == idx else (0, 0, 255, 80) if selected_to == idx else (0, 0, 0, 0)
        draw_rect(rect, color)

def draw_popup(screen, from_idx, to_idx):
    pygame.draw.rect(screen, (240, 240, 240), popup_rect)
    pygame.draw.rect(screen, (0, 0, 0), popup_rect, 2)
    font = pygame.font.SysFont(None, 24)
    text = f"Confirm move from {from_idx} to {to_idx}?"
    rendered = font.render(text, True, (0, 0, 0))
    text_rect = rendered.get_rect(center=(popup_rect.centerx, popup_rect.y + 30))
    screen.blit(rendered, text_rect)
    pygame.draw.rect(screen, (200, 200, 200), confirm_button_rect)
    pygame.draw.rect(screen, (0, 0, 0), confirm_button_rect, 2)
    confirm_text = font.render("Confirm", True, (0, 0, 0))
    confirm_text_rect = confirm_text.get_rect(center=confirm_button_rect.center)
    screen.blit(confirm_text, confirm_text_rect)

def draw_invalid_move_popup(screen):
    popup = pygame.Rect(WINDOW_WIDTH // 2 - 150, WINDOW_HEIGHT // 2 - 30, 300, 60)
    pygame.draw.rect(screen, (255, 200, 200), popup)
    pygame.draw.rect(screen, (200, 0, 0), popup, 2)
    font = pygame.font.SysFont(None, 28)
    text = font.render("Invalid move!", True, (200, 0, 0))
    screen.blit(text, text.get_rect(center=popup.center))