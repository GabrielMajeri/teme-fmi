from copy import deepcopy
from enum import Enum


INFINITY = 2 ** 30

class Square(Enum):
    EMPTY = 0
    BLACK = 1
    WHITE = 2

    def opposite(self):
        if self == self.BLACK:
            return self.WHITE
        elif self == self.WHITE:
            return self.BLACK
        else:
            raise RuntimeError("Cannot invert blank square")


    def is_maximizing(self):
        return self == self.BLACK

    def __str__(self):
        if self == self.BLACK:
            return '░'
        elif self == self.WHITE:
            return '█'
        else:
            return ' '


def is_within_bounds(row, column):
    "Returns True if the given position is within the margins of the board"
    return 0 <= row <= 7 and 0 <= column <= 7


def signum(x):
    "Returns the sign of the number"
    if x < 0:
        return -1
    elif x == 0:
        return 0
    else:
        return 1

class Configuration:
    "Instantaneous configuration of the Reversi board"

    def __init__(self, board):
        self.board = board

    @staticmethod
    def initial():
        board = [
            [Square.EMPTY for _ in range(8)]
            for _ in range(8)
        ]

        board[3][3] = Square.WHITE
        board[3][4] = Square.BLACK

        board[4][3] = Square.BLACK
        board[4][4] = Square.WHITE

        return Configuration(board)

    def possible_moves(self, color):
        """Returns a list of valid moves for the given player.

        The positions are given in a list of 2-tuples, each containing a pair
        (row, column) and a list of end points.
        """
        moves = []

        for row in range(8):
            for column in range(8):
                if self.board[row][column] == Square.EMPTY:
                    ends = self.is_valid(color, row, column)
                    if ends:
                        point = (row, column)
                        moves.append((point, ends))

        return moves

    def is_valid(self, color, row, column):
        """Checks if a given position is a valid spot for placing a piece.

        If it is valid, returns a list with the end points of all the lines
        it would form. Otherwise returns an empty list.
        """

        opposite_color = color.opposite()

        ends = []

        directions = [
            (-1, -1), (-1, 0), (-1, 1),
            (0, -1), (0, 1),
            (1, -1), (1, 0), (1, 1),
        ]

        for direction in directions:
            di, dj = direction

            i, j = row + di, column + dj
            if is_within_bounds(i, j):
                piece = self.board[i][j]

                if piece == Square.EMPTY:
                    # empty square, certainly no line can be formed
                    continue

                if piece == color:
                    # cannot place two same-color pieces next to each other
                    continue

                while is_within_bounds(i, j) and self.board[i][j] == opposite_color:
                    i += di
                    j += dj

                if is_within_bounds(i, j) and self.board[i][j] == color:
                    ends.append((i, j))

        return ends

    def make_move(self, color, row, column, endpoints):
        """Applies a given move to the board.

        Returns a new Configuration object with the updated board.
        """

        new_board = deepcopy(self.board)

        for end_i, end_j in endpoints:
            di, dj = signum(end_i - row), signum(end_j - column)
            i, j = row, column

            while i != end_i or j != end_j:
                new_board[i][j] = color

                i += di
                j += dj

        return Configuration(new_board)

    def count_stones(self, color):
        "Counts the number of stones on the board, for a given player"
        count = 0

        for row in self.board:
            for piece in row:
                if piece == color:
                    count += 1

        return count

    def __repr__(self):
        s = '  '
        for digit in range(8):
            s += str(digit) + ' '
        s += '\n'
        for index, row in enumerate(self.board):
            s += str(index) + ' '
            for column in row:
                s += str(column) + ' '
            s += '\n'
        return s


def minimax(config, player, depth, alpha=-INFINITY, beta=+INFINITY):
    """Returns the relative score for the given player,
    using the minimax algorithm.
    """

    # terminal / leaf node: return the estimated value of this position
    if depth == 0:
        return config.count_stones(Square.BLACK)

    other_player = player.opposite()

    next_moves = config.possible_moves(player)

    # initialize the score / value of this position
    if player.is_maximizing():
        score = -INFINITY
    else:
        score = +INFINITY

    for (row, column), ends in next_moves:
        next_config = config.make_move(player, row, column, ends)
        value = minimax(next_config, other_player, depth - 1, alpha, beta)

        if player.is_maximizing():
            score = max(score, value)
            alpha = max(alpha, value)
        else:
            score = min(score, value)
            beta = min(beta, value)

        if alpha >= beta:
            break

    return score


print("=== REVERSI ===")

# 1 for Black
# 2 for White
HUMAN_COLOR = 1

# Max depth for algorithm
MAX_DEPTH = 5


human_player = Square(HUMAN_COLOR)
computer_player = human_player.opposite()


current_config = Configuration.initial()

current_player = Square.BLACK

while True:
    moves = current_config.possible_moves(current_player)

    if not moves:
        print("Game over")

        human_pieces = current_config.count_stones(human_player)
        computer_pieces = current_config.count_stones(computer_player)

        if human_pieces > computer_pieces:
            print(f"Player {human_player} won!")
        else:
            print(f"Sorry, {computer_player} won.")

        break

    if current_player == human_player:
        print(current_config)

        print("Enter move:")
        try:
            row, column = map(int, input("row column = ").split())
        except ValueError:
            print("Invalid row/column combination!")
            continue

        ends = current_config.is_valid(human_player, row, column)

        if not ends:
            print("Invalid move!")
            continue

        current_config = current_config.make_move(human_player, row, column, ends)
    else:
        configs = [
            current_config.make_move(computer_player, row, column, ends)
            for (row, column), ends in moves
        ]

        moves_and_configs = zip(moves, configs)
        def score_move_and_config(params):
            _, config = params
            return minimax(config, computer_player, MAX_DEPTH)

        if computer_player.is_maximizing():
            func = max
        else:
            func = min

        best_move, best_config = func(moves_and_configs, key=score_move_and_config)

        position, _ = best_move
        print(f"Computer places piece at {position}")

        current_config = best_config

    current_player = current_player.opposite()
