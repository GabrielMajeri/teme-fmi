"""Checkers implementation in Python, with AI."""

import time

from ai import best_move
from model import Configuration, Player
from ui import GUI, TUI

def main():
    max_depth = None
    while max_depth is None:
        print("Choose difficulty")
        print("\t1 = Easy")
        print("\t2 = Medium")
        print("\t3 = Hard")

        difficulty = int(input())

        if difficulty == 1:
            max_depth = 3
        elif difficulty == 2:
            max_depth = 4
        elif difficulty == 3:
            max_depth = 5
        else:
            print("Invalid difficulty")

    prune = None
    while prune is None:
        print("Choose algorithm:")
        print("\t1 = minimax")
        print("\t2 = minimax with alpha-beta pruning")

        algorithm_choice = int(input())
        if algorithm_choice == 1:
            prune = False
        elif algorithm_choice == 2:
            prune = True
        else:
            print("Invalid algorithm")

    human_player = None
    while human_player is None:
        print("Choose player:")
        print("\t1 = Black")
        print("\t2 = White")
        player_choice = int(input())

        if player_choice == 1:
            human_player = Player.BLACK
        elif player_choice == 2:
            human_player = Player.WHITE
        else:
            print("Invalid player")

    computer_player = human_player.opposite()

    print("Human plays as", human_player)
    print("Computer AI plays as", computer_player)

    ui = None
    while ui is None:
        print("Choose user interface type:")
        print("1 = Text")
        print("2 = Graphical")
        ui_choice = int(input())

        if ui_choice == 1:
            ui = TUI()
        elif ui_choice == 2:
            ui = GUI()
        else:
            print("Invalid user interface")

    winner = None
    current_config = Configuration.initial()
    current_player = Player.BLACK

    move_count = 0
    game_start_time = time.time()

    while True:
        move_count += 1

        print("Black's score:", current_config.score())
        ui.render(current_config)

        if not current_config.has_pieces(current_player):
            # the other player won
            winner = current_player.opposite()
            break

        possible_moves = current_config.possible_moves(current_player)

        if not possible_moves:
            # leave winner as `None`, indicating a draw
            break

        if current_player == human_player:
            move = ui.get_next_move(current_config, current_player, possible_moves)

            # provide an option to quit the game
            if move is None:
                break

            current_config = current_config.apply_move(move)
        else:
            print("Computer is thinking...")

            start_time = time.perf_counter()
            current_config = best_move(current_config, computer_player, max_depth, prune)
            end_time = time.perf_counter()

            thinking_time = end_time - start_time
            print(f"Computer spent {thinking_time:.2f} seconds thinking")

        if not current_config.capture_chain:
            current_player = current_player.opposite()

    game_end_time = time.time()
    game_duration = game_end_time - game_start_time

    if winner is None:
        print("The game is a draw")
    else:
        print("Winner:", winner)

    print(f"Game finished after {move_count} moves")
    print(f"Took {int(game_duration)} seconds")

if __name__ == '__main__':
    main()
