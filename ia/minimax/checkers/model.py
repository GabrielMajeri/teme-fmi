"Data model implementation for the game."

from collections import namedtuple
from copy import deepcopy
from enum import IntEnum

def print_board(board):
    print(" ", ' '.join(str(index) for index in range(8)))

    for index, line in enumerate(board):
        print(index, ' '.join(str(piece) for piece in line))

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


Move = namedtuple('Move', ('player', 'from_position', 'to_position'))


def is_within_bounds(index):
    "Checks if the given 0-based index is within the bounds of the game board."
    return 0 <= index <= 7


class Configuration:
    "Node in the game tree."

    def __init__(self, board):
        self.board = board
        self.capture_chain = False
        self.capture_chain_piece = None

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
                    non_capturing_moves.append(Move(player, current_position, new_position))

                # capture
                elif new_position_piece.player() == opposite_player:
                    capture_row = new_row + row_offset
                    capture_column = new_column + column_offset
                    capture_position = (capture_row, capture_column)

                    if (is_within_bounds(capture_row) and
                            is_within_bounds(capture_column)):
                        capture_position_piece = self.board[capture_row][capture_column]
                        if capture_position_piece == Piece.NONE:
                            capturing_moves.append(Move(player, current_position, capture_position))

        return non_capturing_moves, capturing_moves

    def possible_moves(self, player):
        "Returns all the possible moves for the given player."
        # if this is part of a capture chain, only allow connected captures
        if self.capture_chain:
            row, column = self.capture_chain_piece
            _, capturing = self.possible_piece_moves(player, row, column)
            return capturing

        moves = []

        # go through all the pieces
        for row, column in self.player_pieces(player):
            non_capturing, capturing = self.possible_piece_moves(player, row, column)
            moves += non_capturing
            moves += capturing

        return moves

    def apply_move(self, move):
        "Takes a move and applies it to the board, obtaining a new configuration."

        player, (from_row, from_column), (to_row, to_column) = move

        # make a copy of the board
        new_board = deepcopy(self.board)

        # move the piece
        new_board[to_row][to_column] = self.board[from_row][from_column]
        new_board[from_row][from_column] = Piece.NONE

        # upgrade to kings
        if player == Player.BLACK and to_row == 0:
            new_board[to_row][to_column] = Piece.BLACK_KING
        if player == Player.WHITE and to_row == 7:
            new_board[to_row][to_column] = Piece.WHITE_KING

        is_capturing = abs(to_column - from_column) > 1

        # if it's a capturing move, we need to remove the captured piece
        if is_capturing:
            captured_row = (from_row + to_row) // 2
            captured_column = (from_column + to_column) // 2
            new_board[captured_row][captured_column] = Piece.NONE

        new_config = Configuration(new_board)

        # more captures are possible, mark this as a capture chain
        if is_capturing:
            _, capturing_moves = new_config.possible_piece_moves(player, to_row, to_column)
            if capturing_moves:
                new_config.capture_chain = True
                new_config.capture_chain_piece = (to_row, to_column)

        return new_config

    def next_configurations(self, player):
        possible_moves = self.possible_moves(player)
        return [self.apply_move(move) for move in possible_moves]

    def score(self):
        return self.weighted_score()

    def num_pieces_score(self):
        return self.num_pieces(Player.BLACK) - self.num_pieces(Player.WHITE)

    def weighted_score(self):
        black_kings = 0
        white_kings = 0
        black_pieces = 0
        white_pieces = 0

        for row, column in self.player_pieces(Player.BLACK):
            piece = self.board[row][column]
            if piece.is_king():
                black_kings += 1
            else:
                black_pieces += 1

        for row, column in self.player_pieces(Player.WHITE):
            piece = self.board[row][column]
            if piece.is_king():
                white_kings += 1
            else:
                white_pieces += 1

        return (5 * black_kings + black_pieces) - (5 * white_kings + white_pieces)

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
