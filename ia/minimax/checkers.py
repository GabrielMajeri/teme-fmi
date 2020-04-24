"""Checkers implementation in Python, with AI."""

from collections import namedtuple
from enum import IntEnum
import pygame

class Player(IntEnum):
    WHITE = 1
    BLACK = 2

    def opposite(self):
        if self == self.WHITE:
            return self.BLACK
        else:
            return self.WHITE


class Piece(IntEnum):
    NONE = 0
    WHITE = 1
    BLACK = 2
    WHITE_KING = 3
    BLACK_KING = 4

    def player(self):
        if self == self.WHITE or self == self.WHITE_KING:
            return Player.WHITE
        elif self == self.BLACK or self == self.BLACK_KING:
            return Player.BLACK
        raise NotImplementedError

    def is_king(self):
        return self == self.WHITE_KING or self == self.BLACK_KING

    def __str__(self):
        if self == self.WHITE:
            return '●'
        elif self == self.BLACK:
            return '○'
        elif self == self.WHITE_KING:
            return 'A'
        elif self == self.BLACK_KING:
            return 'N'
        else:
            return ' '


Move = namedtuple('Move', ('from_position', 'to_position', 'is_capturing'))


def is_within_bounds(index):
    "Checks if the given 0-based index is within the bounds of the game board"
    return 0 <= index <= 7


class Configuration:
    def __init__(self, board):
        self.board = board

    def possible_piece_moves(self, player, row, column):
        """Returns a list of possible moves for the given piece,
        during the player's turn.
        """
        piece = self.board[row][column]
        current_position = (row, column)
        opposite_player = player.opposite()

        assert piece.player() == player

        if piece.is_king():
            # kings can move backward and forward
            row_offsets = (-1, 1)
        else:
            # normal pieces can only move in the forward direction
            row_offsets = (1 if player == Player.WHITE else -1,)

        moves = []

        for row_offset in row_offsets:
            new_row = row + row_offset

            # see if we have enough space to move to the next row
            if not is_within_bounds(new_row):
                continue

            for column_offset in (-1, 1):
                new_column = column + column_offset

                # make sure we do not fall off the board
                if not is_within_bounds(new_column):
                    continue

                new_position = (new_row, new_column)
                new_position_piece = self.board[new_row][new_column]

                # non-capturing move
                if new_position_piece == Piece.NONE:
                    moves.append(Move(current_position, new_position, is_capturing=False))

                # capture
                elif new_position_piece.player() == opposite_player:
                    capture_row = new_row + row_offset
                    capture_column = new_column + column_offset
                    capture_position = (capture_row, capture_column)

                    if (is_within_bounds(capture_row) and
                        is_within_bounds(capture_column)):
                        moves.append(Move(current_position, capture_position, is_capturing=True))

        return moves

    def possible_moves(self, player):
        "Returns all the possible moves for the given player"
        moves = []
        # go through all the pieces
        for row, line in enumerate(self.board):
            for column, piece in enumerate(line):
                # look for ones belonging to the given player
                if piece != Piece.NONE and piece.player() == player:
                    # collect them in a list
                    moves += self.possible_piece_moves(player, row, column)
        return moves

    @staticmethod
    def initial():
        "Returns the configuration of the starting game board"

        # create an empty board
        initial_board = [[Piece.NONE for _ in range(8)] for _ in range(8)]

        # add the initial pieces
        for row in range(3):
            for column in range((row + 1) % 2, 8, 2):
                initial_board[row][column] = Piece.WHITE

            for column in range(row % 2, 8, 2):
                initial_board[7 - row][column] = Piece.BLACK

        return Configuration(initial_board)

    def __repr__(self):
        header = f"  {' '.join(str(index) for index in range(8))}\n"

        lines = (
            f"{index} {' '.join(str(piece) for piece in row)}"
            for index, row in enumerate(self.board)
        )

        return header + '\n'.join(lines)


COLOR_BACKGROUND = (10, 200, 30)
COLOR_GRID = (0, 0, 0)

COLOR_WHITE_PIECE = (255, 255, 255)
COLOR_BLACK_PIECE = (32, 32, 32)

class GUI:
    """Graphical user interface for playing the game using the mouse"""

    def __init__(self):
        width, height = 640, 480
        self.surface = pygame.display.set_mode((width, height))

        self.board_x = width // 10
        self.board_y = height // 10
        self.board_length = 400

        self.cell_length = self.board_length // 8
        self.cell_gutter = int(0.15 * self.cell_length)
        self.piece_radius = self.cell_length // 2 - self.cell_gutter

        pygame.display.set_caption("Checkers")

    def redraw(self, config):
        # clear the background
        self.surface.fill(COLOR_BACKGROUND)

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

        for row, line in enumerate(config.board):
            for column, piece in enumerate(line):
                if piece != Piece.NONE:
                    self.draw_piece(piece.player(), row, column)

        pygame.display.flip()


    def draw_piece(self, player, row, column):
        if player == Player.WHITE:
            color = COLOR_WHITE_PIECE
        else:
            color = COLOR_BLACK_PIECE

        x = self.board_x + self.cell_gutter + self.piece_radius + column * self.cell_length
        y = self.board_y + self.cell_gutter + self.piece_radius + row * self.cell_length

        position = (x, y)

        pygame.draw.circle(self.surface, color, position, self.piece_radius)


current_configuration = Configuration.initial()

print(current_configuration)
print(current_configuration.possible_moves(Player.WHITE))

gui = GUI()
gui.redraw(current_configuration)

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            exit()
