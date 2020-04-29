"""Checkers implementation in Python, with AI."""

from abc import ABC, abstractmethod
from collections import namedtuple
from copy import deepcopy
from enum import IntEnum
from math import inf
import pygame

INFINITY = inf

class Player(IntEnum):
    "Enumeration of the possible players."

    BLACK = 1
    WHITE = 2

    def opposite(self):
        "Returns the opposite player of this one."
        return self.WHITE if self == self.BLACK else self.BLACK

    def is_maximizing(self):
        "Returns true if this is the maximizing player."
        return self == self.BLACK


class Piece(IntEnum):
    "Enumeration of the possible pieces."

    NONE = 0
    BLACK = 1
    WHITE = 2
    BLACK_KING = 3
    WHITE_KING = 4

    def player(self):
        "Returns the player owning this piece."
        if self in (self.BLACK, self.BLACK_KING):
            return Player.BLACK
        if self in (self.WHITE, self.WHITE_KING):
            return Player.WHITE
        raise NotImplementedError

    def is_king(self):
        "Predicate for checking if this piece is a king."
        return self in (self.BLACK_KING, self.WHITE_KING)

    def __str__(self):
        if self == self.BLACK:
            return '○'
        if self == self.WHITE:
            return '●'
        if self == self.BLACK_KING:
            return 'N'
        if self == self.WHITE_KING:
            return 'A'
        # Empty cell
        return ' '


Move = namedtuple('Move', ('from_position', 'to_position'))


def is_within_bounds(index):
    "Checks if the given 0-based index is within the bounds of the game board."
    return 0 <= index <= 7


class Configuration:
    "Node in the game tree."

    def __init__(self, board):
        self.board = board
        self.capture_chain = False

    def player_pieces(self, player):
        "Generator which yields the positions of the pieces belonging to the given player."
        for row, line in enumerate(self.board):
            for column, piece in enumerate(line):
                # look for ones belonging to the given player
                if piece != Piece.NONE and piece.player() == player:
                    yield row, column

    def num_pieces(self, player):
        "Returns number of pieces belonging to a certain player"
        return sum(1 for _ in self.player_pieces(player))

    def has_pieces(self, player):
        "Predicate for checking if a given player has any pieces left."

        for _ in self.player_pieces(player):
            # if the player has at least one piece,
            # we will enter this `for` loop
            return True

        # player has no pieces
        return False

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

        non_capturing_moves = []
        capturing_moves = []

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
                    non_capturing_moves.append(Move(current_position, new_position))

                # capture
                elif new_position_piece.player() == opposite_player:
                    capture_row = new_row + row_offset
                    capture_column = new_column + column_offset
                    capture_position = (capture_row, capture_column)

                    if (is_within_bounds(capture_row) and
                            is_within_bounds(capture_column)):
                        capture_position_piece = self.board[capture_row][capture_column]
                        if capture_position_piece == Piece.NONE:
                            capturing_moves.append(Move(current_position, capture_position))

        return non_capturing_moves, capturing_moves

    def possible_moves(self, player):
        "Returns all the possible moves for the given player."
        moves = []

        # go through all the pieces
        for row, column in self.player_pieces(player):
            non_capturing, capturing = self.possible_piece_moves(player, row, column)

            # also allow non-capturing moves if this is not part of a capture chain
            if not self.capture_chain:
                moves += non_capturing

            moves += capturing

        return moves

    def has_capturing_moves(self, player):
        for row, column in self.player_pieces(player):
            _, capturing = self.possible_piece_moves(player, row, column)
            if capturing:
                return True
        return False

    def apply_move(self, move):
        "Takes a move and applies it to the board, obtaining a new configuration."

        (from_row, from_column), (to_row, to_column) = move

        # make a copy of the board
        new_board = deepcopy(self.board)

        # move the piece
        new_board[to_row][to_column] = self.board[from_row][from_column]
        new_board[from_row][from_column] = Piece.NONE

        is_capturing = abs(to_column - from_column) > 1

        # if it's a capturing move, we need to remove the captured piece
        if is_capturing:
            captured_row = (from_row + to_row) // 2
            captured_column = (from_column + to_column) // 2
            new_board[captured_row][captured_column] = Piece.NONE

        new_config = Configuration(new_board)

        # more captures are possible, mark this as a capture chain
        if is_capturing:
            player = self.board[from_row][from_column].player()
            if new_config.has_capturing_moves(player):
                new_config.capture_chain = True

        return new_config

    def next_configurations(self, moves):
        return [self.apply_move(move) for move in moves]

    def score(self):
        return self.num_pieces(Player.BLACK) - self.num_pieces(Player.WHITE)

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


def minimax(config, player, depth):
    if depth == 0:
        return config.score()

    possible_moves = config.possible_moves(player)
    next_configs = config.next_configurations(possible_moves)

    opposite_player = player.opposite()

    if player.is_maximizing():
        score = -INFINITY
    else:
        score = +INFINITY

    for next_config in next_configs:
        if next_config.capture_chain:
            next_player = player
        else:
            next_player = opposite_player

        next_score = minimax(next_config, next_player, depth - 1)

        if player.is_maximizing():
            score = max(score, next_score)
        else:
            score = min(score, next_score)

    return score

class UI(ABC):
    "Base class for a checkers user interface"

    @abstractmethod
    def render(self, config):
        ...


class TUI(UI):
    "Text user interface for playing the game in the terminal"

    def render(self, config):
        print(config)


COLOR_BACKGROUND = (10, 200, 30)
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

        pygame.display.set_caption("Checkers")

    def render(self, config):
        # clear the background
        self.clear()

        # draw the board
        self.draw_board_grid()

        # draw the pieces
        self.draw_pieces(config.board)

        # present the updated draw buffer
        pygame.display.flip()

    def clear(self):
        self.surface.fill(COLOR_BACKGROUND)

    def draw_board_grid(self):
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
                    self.draw_piece(piece.player(), row, column)

    def draw_piece(self, player, row, column):
        if player == Player.WHITE:
            color = COLOR_WHITE_PIECE
        else:
            color = COLOR_BLACK_PIECE

        x = self.board_x + self.cell_gutter + self.piece_radius + column * self.cell_length
        y = self.board_y + self.cell_gutter + self.piece_radius + row * self.cell_length

        position = (x, y)

        pygame.draw.circle(self.surface, color, position, self.piece_radius)


def main():
    human_player = None
    while not human_player:
        print("Choose player:")
        print("\t1 = Black")
        print("\t2 = White")
        #player_choice = int(input())
        player_choice = 1

        if player_choice == 1:
            human_player = Player.WHITE
        elif player_choice == 2:
            human_player = Player.BLACK
        else:
            print("Invalid player")

    computer_player = human_player.opposite()

    print("Human plays as", human_player)
    print("Computer AI plays as", computer_player)

    max_depth = 1

    ui = None
    while not ui:
        print("Choose user interface type:")
        print("1 = Text")
        print("2 = Graphical")
        #ui_choice = int(input())
        ui_choice = 1

        if ui_choice == 1:
            ui = TUI()
        elif ui_choice == 2:
            ui = GUI()
        else:
            print("Invalid user interface")

    winner = None
    current_configuration = Configuration.initial()
    current_player = Player.BLACK

    while True:
        print("Black's score:", current_configuration.score())
        ui.render(current_configuration)

        if not current_configuration.has_pieces(current_player):
            # the other player won
            winner = current_player.opposite()
            break

        possible_moves = current_configuration.possible_moves(current_player)

        if not possible_moves:
            # leave winner as `None`, indicating a draw
            break

        if current_player == human_player:
            board = current_configuration.board
            while True:
                print("Choose which piece to move:")
                from_row, from_column = map(int, input().split())
                from_position = (from_row, from_column)

                piece = board[from_row][from_column]
                if piece == Piece.NONE or piece.player() != current_player:
                    print("Invalid piece")
                    continue

                print("Choose where to move to:")
                to_row, to_column = map(int, input().split())
                to_position = (to_row, to_column)

                move = Move(from_position, to_position)

                if move not in possible_moves:
                    print("Invalid move")
                    continue

                break

            current_configuration = current_configuration.apply_move(move)
        else:
            print("Computer is thinking...")

            next_configs = current_configuration.next_configurations(possible_moves)

            scores = {}
            for config in next_configs:
                # for every possible move, compute a score
                score = minimax(config, computer_player, max_depth)
                scores[config] = score

            if computer_player.is_maximizing():
                optimum_func = max
            else:
                optimum_func = min

            current_configuration = optimum_func(next_configs, key=lambda cfg: scores[cfg])

        if not current_configuration.capture_chain:
            current_player = current_player.opposite()

    print("Winner:", winner)

    """
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit()
    """

if __name__ == '__main__':
    main()
