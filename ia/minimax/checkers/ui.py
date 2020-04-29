"""User interface for the game.

Provides input/output methods for both graphical and text-based interfaces.
"""

from abc import ABC, abstractmethod
import pygame

from model import Move, Piece, Player, print_board

class UI(ABC):
    "Base class for a checkers user interface."

    @abstractmethod
    def render(self, config):
        "Display the current configuration of the board."
        ...

    @abstractmethod
    def get_next_move(self, config, player, possible_moves):
        "Get the next move from the user."
        ...

class TUI(UI):
    "Text user interface for playing the game in the terminal"

    def render(self, config):
        print_board(config.board)

    def get_next_move(self, config, player, possible_moves):
        while True:
            print("Choose which piece to move and where to (x1 y1 x2 y2).")
            print("Or type `exit` to resign:")
            line = input()

            if line == 'exit':
                return None

            try:
                from_row, from_column, to_row, to_column = map(int, line.split())
            except ValueError:
                print("Invalid input")
                continue

            from_position = (from_row, from_column)
            to_position = (to_row, to_column)

            move = Move(player, from_position, to_position)

            if move not in possible_moves:
                print("Invalid move")
                continue

            return move

COLOR_BACKGROUND = (10, 200, 30)
COLOR_SELECTION = (32, 16, 128, 64)
COLOR_GRID = (0, 0, 0)

COLOR_WHITE_PIECE = (255, 255, 255)
COLOR_BLACK_PIECE = (32, 32, 32)

class GUI(UI):
    "Graphical user interface for playing the game using the mouse"

    def __init__(self):
        width, height = 640, 480
        self.surface = pygame.display.set_mode((width, height))

        self.board_x = width // 10
        self.board_y = height // 10
        self.board_length = 400

        self.cell_length = self.board_length // 8
        self.cell_gutter = int(0.15 * self.cell_length)
        self.piece_radius = self.cell_length // 2 - self.cell_gutter

        self.selected_piece = None

        pygame.display.set_caption("Checkers")

    def render(self, config):
        # clear the background
        self.clear()

        # draw the board
        self.draw_board_grid()

        if self.selected_piece:
            row, column = self.selected_piece

            x = self.board_x + self.cell_length * column
            y = self.board_y + self.cell_length * row

            selected_rect = (x, y, self.cell_length, self.cell_length)
            pygame.draw.rect(self.surface, COLOR_SELECTION, selected_rect, 0)

        # draw the pieces
        self.draw_pieces(config.board)

        # present the updated draw buffer
        pygame.display.flip()

    def clear(self):
        "Clears the window background."
        self.surface.fill(COLOR_BACKGROUND)

    def draw_board_grid(self):
        "Draws the grid lines for the play board."
        board_rect = (self.board_x, self.board_y, self.board_length, self.board_length)
        pygame.draw.rect(self.surface, COLOR_GRID, board_rect, 1)

        for idx in range(1, 8):
            x = self.board_x + idx * self.cell_length
            start_pos = (x, self.board_y)
            end_pos = (x, self.board_y + self.board_length)
            pygame.draw.line(self.surface, COLOR_GRID, start_pos, end_pos)

            y = self.board_y + idx * self.cell_length
            start_pos = (self.board_x, y)
            end_pos = (self.board_x + self.board_length, y)
            pygame.draw.line(self.surface, COLOR_GRID, start_pos, end_pos)

    def draw_pieces(self, board):
        for row, line in enumerate(board):
            for column, piece in enumerate(line):
                if piece != Piece.NONE:
                    self.draw_piece(piece.player(), row, column, piece.is_king())

    def draw_piece(self, player, row, column, is_king):
        if player == Player.WHITE:
            color = COLOR_WHITE_PIECE
        else:
            color = COLOR_BLACK_PIECE

        x = self.board_x + self.cell_gutter + self.piece_radius + column * self.cell_length
        y = self.board_y + self.cell_gutter + self.piece_radius + row * self.cell_length

        position = (x, y)

        pygame.draw.circle(self.surface, color, position, self.piece_radius)

        if is_king:
            pygame.draw.circle(self.surface, (255, 0, 0), position, self.piece_radius // 3)

    def get_next_move(self, config, player, possible_moves):
        pygame.event.clear()
        while True:
            self.render(config)

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    return None
                if event.type == pygame.MOUSEBUTTONDOWN:
                    x, y = event.pos

                    x = x - self.board_x
                    y = y - self.board_y

                    if ((0 < x < self.board_length) and
                            (0 < y < self.board_length)):
                        column = x // self.cell_length
                        row = y // self.cell_length

                        position = (row, column)

                        if self.selected_piece:
                            from_position = self.selected_piece
                            to_position = position

                            if config.board[row][column] != Piece.NONE:
                                self.selected_piece = position
                                continue

                            self.selected_piece = None

                            move = Move(player, from_position, to_position)

                            if move not in possible_moves:
                                continue

                            return move

                        self.selected_piece = position
