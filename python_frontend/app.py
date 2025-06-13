# app.py
import pygame
import sys
from prolog_bridge import (
    get_current_board_state, roll_dice, get_dice, perform_move, reset_board, ai_move
)
from constants import *
from render import (
    get_triangle_rect, get_bar_rect, get_off_rect,
    draw_board, draw_checkers, draw_bar_and_off,
    draw_dice, draw_triangle_buttons, draw_popup,
    draw_invalid_move_popup
)

is_user_turn = True  # Tracks whose turn it is

def main():
    global selected_from, selected_to, show_popup, show_invalid_move_popup, invalid_popup_timer
    global has_rolled_dice, is_user_turn

    reset_board()

    state = get_current_board_state()
    if not state:
        print("Failed to load board state.")
        return
    dice_values = []

    white_checkers, black_checkers, bar_white, bar_black, off_white, off_black = state

    pygame.init()
    screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
    pygame.display.set_caption("Backgammon")
    clock = pygame.time.Clock()

    running = True
    while running:
        current_time = pygame.time.get_ticks()

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    sys.exit()
            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                pos = pygame.mouse.get_pos()
                if roll_button_rect.collidepoint(pos) and not has_rolled_dice:
                    roll_dice()
                    dice_values = get_dice()
                    has_rolled_dice = True
                elif not is_user_turn and ai_move_button_rect.collidepoint(pos) and has_rolled_dice:
                    updated = ai_move("black")
                    if updated:
                        white_checkers, black_checkers, bar_white, bar_black, off_white, off_black = updated
                        dice_values = get_dice()
                    if not dice_values:
                        is_user_turn = True
                        has_rolled_dice = False
                elif show_popup and selected_from is not None and selected_to is not None:
                    if confirm_button_rect.collidepoint(pos):
                        fr = 25 if selected_from == BAR_WHITE_IDX else selected_from
                        to = 27 if selected_to == OFF_WHITE_IDX else selected_to
                        updated = perform_move("white", fr, to)
                        if updated:
                            white_checkers, black_checkers, bar_white, bar_black, off_white, off_black = updated
                            dice_values = get_dice()
                        else:
                            show_invalid_move_popup = True
                            invalid_popup_timer = current_time
                        selected_from = None
                        selected_to = None
                        show_popup = False
                        if not dice_values:
                            is_user_turn = False
                            has_rolled_dice = False
                else:
                    for i in range(1, 25):
                        if get_triangle_rect(i).collidepoint(pos):
                            if selected_from is None:
                                selected_from = i
                            elif selected_to is None:
                                selected_to = i
                                show_popup = True
                            break
                    if get_bar_rect(True).collidepoint(pos):
                        if selected_from is None:
                            selected_from = BAR_WHITE_IDX
                    elif get_off_rect(True).collidepoint(pos):
                        if selected_from is not None and selected_to is None:
                            selected_to = OFF_WHITE_IDX
                            show_popup = True

        draw_board(screen)
        draw_checkers(screen, white_checkers, black_checkers)
        draw_bar_and_off(screen, bar_white, bar_black, off_white, off_black)

        if has_rolled_dice:
            draw_dice(screen, dice_values)
        draw_triangle_buttons(screen, selected_from, selected_to)

        # Roll button
        pygame.draw.rect(screen, (173, 216, 230) if not has_rolled_dice else (200, 200, 200), roll_button_rect)
        pygame.draw.rect(screen, (0, 0, 0), roll_button_rect, 2)
        font = pygame.font.SysFont(None, 24)
        roll_text = font.render("Roll Dice", True, (0, 0, 0))
        screen.blit(roll_text, roll_text.get_rect(center=roll_button_rect.center))

        # AI move button
        if not is_user_turn and has_rolled_dice:
            pygame.draw.rect(screen, (240, 230, 140), ai_move_button_rect)
            pygame.draw.rect(screen, (0, 0, 0), ai_move_button_rect, 2)
            ai_text = font.render("Make AI Move", True, (0, 0, 0))
            screen.blit(ai_text, ai_text.get_rect(center=ai_move_button_rect.center))

        if show_popup:
            draw_popup(screen, selected_from, selected_to)

        if show_invalid_move_popup and current_time - invalid_popup_timer < 1500:
            draw_invalid_move_popup(screen)
        else:
            show_invalid_move_popup = False

        pygame.display.flip()
        clock.tick(30)

    pygame.quit()

if __name__ == "__main__":
    main()
