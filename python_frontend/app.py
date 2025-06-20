# app.py
import sys
import pygame

from prolog_bridge import (
    get_current_board_state,
    roll_dice,
    get_dice,
    perform_move,
    reset_board,
    ai_move,
    has_available_moves,
    is_winner,
)
from constants import *
from render import (
    get_triangle_rect,
    get_bar_rect,
    get_off_rect,
    draw_board,
    draw_checkers,
    draw_bar_and_off,
    draw_dice,
    draw_triangle_buttons,
    draw_popup,
    draw_invalid_move_popup,
    draw_no_moves_popup,
    draw_winner_popup,
)

is_user_turn = True
show_no_moves_popup = False
no_moves_player = None


def main():
    global selected_from, selected_to, show_popup
    global show_invalid_move_popup, invalid_popup_timer
    global has_rolled_dice, is_user_turn
    global show_no_moves_popup, no_moves_player
    global winner_player

    reset_board()
    state = get_current_board_state()
    if not state:
        print("Failed to load board state.")
        return

    white_checkers, black_checkers, bar_white, bar_black, off_white, off_black = state
    dice_values = []

    pygame.init()
    screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
    pygame.display.set_caption("Backgammon")
    clock = pygame.time.Clock()

    running = True
    while running:
        now = pygame.time.get_ticks()

        for event in pygame.event.get():
            if event.type == pygame.QUIT or (event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE):
                running = False
                break

            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                pos = pygame.mouse.get_pos()

                if winner_player:
                    continue

                if show_no_moves_popup:
                    if no_moves_button_rect.collidepoint(pos):
                        is_user_turn = (no_moves_player == "black")
                        has_rolled_dice = False
                        dice_values = []
                        selected_from = selected_to = None
                        show_no_moves_popup = False
                        no_moves_player = None
                    continue

                if show_popup and selected_from is not None and selected_to is not None:
                    if confirm_button_rect.collidepoint(pos):
                        fr = 25 if selected_from == BAR_WHITE_IDX else selected_from
                        to = 27 if selected_to == OFF_WHITE_IDX else selected_to
                        updated = perform_move("white", fr, to)
                        if updated:
                            white_checkers, black_checkers, bar_white, bar_black, off_white, off_black = updated
                            dice_values = get_dice()
                            if is_winner("white"):
                                winner_player = "white"
                        else:
                            show_invalid_move_popup = True
                            invalid_popup_timer = now

                        selected_from = selected_to = None
                        show_popup = False

                        if not dice_values:
                            is_user_turn = False
                            has_rolled_dice = False
                    continue

                if roll_button_rect.collidepoint(pos) and not has_rolled_dice:
                    roll_dice()
                    dice_values = get_dice()
                    has_rolled_dice = True
                    current = "white" if is_user_turn else "black"
                    if not has_available_moves(current):
                        show_no_moves_popup = True
                        no_moves_player = current
                    continue

                if (not is_user_turn) and has_rolled_dice and ai_move_button_rect.collidepoint(pos):
                    updated = ai_move("black")
                    if updated:
                        white_checkers, black_checkers, bar_white, bar_black, off_white, off_black = updated
                        dice_values = get_dice()
                        if is_winner("black"):
                            winner_player = "black"

                    if not dice_values:
                        is_user_turn = True
                        has_rolled_dice = False
                    elif not has_available_moves("black"):
                        show_no_moves_popup = True
                        no_moves_player = "black"
                    continue

                if is_user_turn and has_rolled_dice:
                    for i in range(1, 25):
                        if get_triangle_rect(i).collidepoint(pos):
                            if selected_from is None:
                                selected_from = i
                            elif selected_to is None:
                                selected_to = i
                                show_popup = True
                            break
                    if selected_from is None and get_bar_rect(True).collidepoint(pos):
                        selected_from = BAR_WHITE_IDX
                    elif selected_from is not None and selected_to is None and get_off_rect(True).collidepoint(pos):
                        selected_to = OFF_WHITE_IDX
                        show_popup = True

        if is_user_turn and has_rolled_dice and not show_no_moves_popup and not winner_player:
            if not has_available_moves("white"):
                show_no_moves_popup = True
                no_moves_player = "white"

        draw_board(screen)
        draw_checkers(screen, white_checkers, black_checkers)
        draw_bar_and_off(screen, bar_white, bar_black, off_white, off_black)

        if has_rolled_dice:
            draw_dice(screen, dice_values)

        draw_triangle_buttons(screen, selected_from, selected_to)

        pygame.draw.rect(screen, (173, 216, 230) if not has_rolled_dice else (200, 200, 200), roll_button_rect)
        pygame.draw.rect(screen, (0, 0, 0), roll_button_rect, 2)
        font = pygame.font.SysFont(None, 24)
        roll_txt = font.render("Roll Dice", True, (0, 0, 0))
        screen.blit(roll_txt, roll_txt.get_rect(center=roll_button_rect.center))

        if (not is_user_turn) and has_rolled_dice and not show_no_moves_popup and not winner_player:
            pygame.draw.rect(screen, (240, 230, 140), ai_move_button_rect)
            pygame.draw.rect(screen, (0, 0, 0), ai_move_button_rect, 2)
            ai_txt = font.render("Make AI Move", True, (0, 0, 0))
            screen.blit(ai_txt, ai_txt.get_rect(center=ai_move_button_rect.center))

        if show_popup:
            draw_popup(screen, selected_from, selected_to)

        if show_invalid_move_popup and now - invalid_popup_timer < 1500:
            draw_invalid_move_popup(screen)
        else:
            show_invalid_move_popup = False

        if show_no_moves_popup:
            draw_no_moves_popup(screen, no_moves_player)

        if winner_player:
            draw_winner_popup(screen, winner_player)

        pygame.display.flip()
        clock.tick(30)

    pygame.quit()


if __name__ == "__main__":
    main()
