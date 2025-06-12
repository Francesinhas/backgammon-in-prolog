import pygame

# --- Constants ---
WINDOW_WIDTH = 900
WINDOW_HEIGHT = 600
BOARD_COLOR = (222, 184, 135)
POINT_WIDTH = 60
POINT_HEIGHT = 250
CHECKER_RADIUS = 20
BAR_WIDTH = CHECKER_RADIUS * 2  # exactly one checker wide
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
NUM_TRIANGLES_PER_SIDE = 6

# Derived layout
TOTAL_PLAY_WIDTH = 2 * NUM_TRIANGLES_PER_SIDE * POINT_WIDTH + BAR_WIDTH
MARGIN_X = (WINDOW_WIDTH - TOTAL_PLAY_WIDTH) // 2
LEFT_ZONE_START = MARGIN_X
BAR_START_X = LEFT_ZONE_START + NUM_TRIANGLES_PER_SIDE * POINT_WIDTH
RIGHT_ZONE_START = BAR_START_X + BAR_WIDTH

def get_point_x(index):
    if index < 6:
        # Bottom-right (points 1–6)
        return RIGHT_ZONE_START + (5 - index) * POINT_WIDTH
    elif index < 12:
        # Bottom-left (points 7–12)
        return LEFT_ZONE_START + (11 - index) * POINT_WIDTH
    elif index < 18:
        # Top-left (points 13–18)
        return LEFT_ZONE_START + (index - 12) * POINT_WIDTH
    else:
        # Top-right (points 19–24)
        return RIGHT_ZONE_START + (index - 18) * POINT_WIDTH

def draw_board(screen):
    screen.fill(BOARD_COLOR)

    # Draw triangles
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

    # Draw bar same width as checker
    pygame.draw.rect(screen, (105, 105, 105), (
        BAR_START_X,
        0,
        BAR_WIDTH,
        WINDOW_HEIGHT
    ))

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

def main(white_checkers, black_checkers):
    pygame.init()
    screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
    pygame.display.set_caption("Backgammon")
    clock = pygame.time.Clock()

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        draw_board(screen)
        draw_checkers(screen, white_checkers, black_checkers)

        pygame.display.flip()
        clock.tick(30)

    pygame.quit()

if __name__ == "__main__":
    white = [0]*24
    black = [0]*24

    white[0] = 2     # Point 1
    white[11] = 5    # Point 12
    white[16] = 3    # Point 17
    black[23] = 2    # Point 24
    black[12] = 5    # Point 13
    black[7] = 3     # Point 8

    main(white, black)
