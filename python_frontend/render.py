import pygame
import sys

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

# Derived layout
TOTAL_PLAY_WIDTH = 2 * NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + BAR_WIDTH
MARGIN_X = (WINDOW_WIDTH - TOTAL_PLAY_WIDTH) // 2
LEFT_ZONE_START = MARGIN_X
BAR_START_X = LEFT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH
RIGHT_ZONE_START = BAR_START_X + BAR_WIDTH

# Move selection state
selected_from = None
selected_to = None
show_popup = False
popup_rect = pygame.Rect(WINDOW_WIDTH//2 - 120, WINDOW_HEIGHT//2 - 50, 240, 100)
confirm_button_rect = pygame.Rect(popup_rect.x + 60, popup_rect.y + 50, 120, 30)

def get_point_x(index):
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
    y = 0 if index >= 12 else WINDOW_HEIGHT - POINT_HEIGHT
    return pygame.Rect(x, y, POINT_WIDTH, POINT_HEIGHT)

def draw_board(screen):
    screen.fill(BOARD_COLOR)
    for i in range(24):
        x = get_point_x(i)
        y = 0 if i >= 12 else WINDOW_HEIGHT
        direction = 1 if i >= 12 else -1
        color = (139, 69, 19) if i % 2 == 0 else (160, 82, 45)
        pygame.draw.polygon(screen, color, [
            (x, y),
            (x + POINT_WIDTH, y),
            (x + POINT_WIDTH // 2, y + direction * POINT_HEIGHT)
        ])
    pygame.draw.rect(screen, (105, 105, 105), (BAR_START_X, 0, BAR_WIDTH, WINDOW_HEIGHT))

def draw_checkers(screen, white_checkers, black_checkers):
    for i in range(24):
        x = get_point_x(i) + POINT_WIDTH // 2
        if i < 12:
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

def draw_triangle_buttons(screen, selected_from):
    for i in range(24):
        rect = get_triangle_rect(i)
        color = (255, 255, 0, 80) if selected_from == i else (0, 0, 0, 0)
        s = pygame.Surface((POINT_WIDTH, POINT_HEIGHT), pygame.SRCALPHA)
        s.fill(color)
        screen.blit(s, (rect.x, rect.y))

def draw_popup(screen, from_idx, to_idx):
    pygame.draw.rect(screen, (240, 240, 240), popup_rect)
    pygame.draw.rect(screen, (0, 0, 0), popup_rect, 2)
    font = pygame.font.SysFont(None, 24)
    text = font.render(f"Confirm move from {from_idx + 1} to {to_idx + 1}?", True, (0, 0, 0))
    text_rect = text.get_rect(center=(popup_rect.centerx, popup_rect.y + 25))
    screen.blit(text, text_rect)
    pygame.draw.rect(screen, (200, 200, 200), confirm_button_rect)
    pygame.draw.rect(screen, (0, 0, 0), confirm_button_rect, 2)
    confirm_text = font.render("Confirm", True, (0, 0, 0))
    confirm_text_rect = confirm_text.get_rect(center=confirm_button_rect.center)
    screen.blit(confirm_text, confirm_text_rect)

def main(white_checkers, black_checkers, bar_white, bar_black, off_white, off_black, dice_values):
    global selected_from, selected_to, show_popup
    pygame.init()
    screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
    pygame.display.set_caption("Backgammon")
    clock = pygame.time.Clock()

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

            elif event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                pygame.quit()
                sys.exit()

            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                pos = pygame.mouse.get_pos()
                if show_popup:
                    if confirm_button_rect.collidepoint(pos):
                        print(f"Confirmed move from {selected_from + 1} to {selected_to + 1}")
                        selected_from = None
                        selected_to = None
                        show_popup = False
                else:
                    for i in range(24):
                        if get_triangle_rect(i).collidepoint(pos):
                            if selected_from is None:
                                selected_from = i
                            elif selected_to is None:
                                selected_to = i
                                show_popup = True
                            break

        draw_board(screen)
        draw_checkers(screen, white_checkers, black_checkers)
        draw_bar_and_off(screen, bar_white, bar_black, off_white, off_black)
        draw_dice(screen, dice_values)
        draw_triangle_buttons(screen, selected_from)
        if show_popup and selected_from is not None and selected_to is not None:
            draw_popup(screen, selected_from, selected_to)

        pygame.display.flip()
        clock.tick(30)

    pygame.quit()

if __name__ == "__main__":
    white = [0] * 24
    black = [0] * 24
    white[0] = 2
    white[11] = 5
    white[16] = 3
    black[23] = 2
    black[12] = 5
    black[7] = 3

    bar_white = 2
    bar_black = 3
    off_white = 6
    off_black = 4

    dice = [6, 6, 6, 6]

    main(white, black, bar_white, bar_black, off_white, off_black, dice)
