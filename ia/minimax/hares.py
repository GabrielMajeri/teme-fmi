from collections import defaultdict
from copy import deepcopy
from enum import IntEnum


INFINITY = 2 ** 30

class Player(IntEnum):
    HARE = 1
    HOUNDS = 2

    def opposite(self):
        if self == self.HARE:
            return self.HOUNDS
        else:
            return self.HARE

    def is_maximizing(self):
        return self == self.HARE


NEIGHBORS = defaultdict(set)
BACKWARDS = set()
DISTANCES = {
    0: 4,
    1: 3,
    2: 3,
    3: 3,
    4: 2,
    5: 2,
    6: 2,
    7: 1,
    8: 1,
    9: 1,
    10: 0
}

def add_edge(u, v):
    NEIGHBORS[u].add(v)
    NEIGHBORS[v].add(u)

def add_backwards(u, v):
    BACKWARDS.add((u, v))


add_edge(0, 1)
add_edge(0, 2)
add_edge(0, 3)

add_edge(1, 2)
add_edge(1, 4)
add_edge(1, 5)
add_backwards(1, 0)

add_edge(2, 5)
add_edge(2, 3)
add_backwards(2, 0)

add_edge(3, 5)
add_edge(3, 6)
add_backwards(3, 0)

add_edge(4, 5)
add_edge(4, 7)
add_backwards(4, 1)

add_edge(5, 6)
add_edge(5, 7)
add_edge(5, 8)
add_edge(5, 9)
add_backwards(5, 1)
add_backwards(5, 2)
add_backwards(5, 3)

add_edge(6, 9)
add_backwards(6, 3)

add_edge(7, 8)
add_edge(7, 10)
add_backwards(7, 4)
add_backwards(7, 5)

add_edge(8, 9)
add_edge(8, 10)
add_backwards(8, 5)

add_edge(9, 10)
add_backwards(9, 5)
add_backwards(9, 6)

add_backwards(10, 7)
add_backwards(10, 8)
add_backwards(10, 9)


def format_board(values):
    return (f"\n  {values[1]}-{values[4]}-{values[7]}\n" +
             " /|\\|/|\\\n" +
            f"{values[0]}-{values[2]}-{values[5]}-{values[8]}-{values[10]}\n" +
            " \\|/|\\|/\n" +
            f"  {values[3]}-{values[6]}-{values[9]}\n")

class Configuration:
    def __init__(self, hare_position, hound_positions):
        self.hare_position = hare_position
        self.hound_positions = hound_positions

    @staticmethod
    def initial():
        return Configuration(
            10,
            (0, 1, 3),
        )

    def __repr__(self):
        board = ['*'] * 11

        board[self.hare_position] = 'i'
        for hound_position in self.hound_positions:
            board[hound_position] = 'c'

        return format_board(board)

    def hare_moves(self):
        moves = []

        for neighbor in NEIGHBORS[self.hare_position]:
            if neighbor in self.hound_positions:
                continue

            new_hare_position = neighbor

            moves.append(Configuration(new_hare_position, self.hound_positions))

        return moves

    def hound_moves(self):
        moves = []

        for hound in range(3):
            hound_position = self.hound_positions[hound]

            # lambda which checks that a neighbor position is not backwards
            # relative to the hound's position
            not_backwards = lambda neighbor: (hound_position, neighbor) not in BACKWARDS

            for neighbor in filter(not_backwards, NEIGHBORS[hound_position]):
                if neighbor == self.hare_position or neighbor in self.hound_positions:
                    continue

                new_hound_positions = self.hound_positions[:hound] + (neighbor,) + self.hound_positions[hound + 1:]
                moves.append(Configuration(self.hare_position, new_hound_positions))

        return moves

    def is_hare_escaped(self):
        return self.hare_position == 0

    def is_hare_cornered(self):
        return self.hare_position == 10 and set(self.hound_positions) == {7, 8, 9}

    def heuristic(self):
        if self.is_hare_escaped():
            return INFINITY

        if self.is_hare_cornered():
            return -INFINITY

        return DISTANCES[self.hare_position] + sum(
            DISTANCES[position]
            for position in self.hound_positions
        )


def minimax(config, player, depth, alpha=-INFINITY, beta=+INFINITY):
    """Returns the relative score for the given player,
    using the minimax algorithm.
    """

    # terminal / leaf node: return the estimated value of this position
    if depth == 0:
        return config.heuristic()

    other_player = player.opposite()

    if player == Player.HARE:
        next_moves = config.hare_moves()
    else:
        next_moves = config.hound_moves()

    # initialize the score / value of this position
    if player.is_maximizing():
        score = -INFINITY
    else:
        score = +INFINITY

    for next_config in next_moves:
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


HUMAN_PLAYER = Player.HOUNDS
MAX_DEPTH = 10


computer_player = HUMAN_PLAYER.opposite()

current_player = Player.HOUNDS
current_configuration = Configuration.initial()

while True:
    if current_configuration.is_hare_escaped():
        print("Hare won!")
        break
    if current_configuration.is_hare_cornered():
        print("Hounds won!")
        break

    if current_player == HUMAN_PLAYER:
        print(format_board(list(range(11))))

        print(current_configuration)

        try:
            if current_player == Player.HARE:
                next_move = int(input("hare position = "))

                if next_move == current_configuration.hare_position:
                    raise ValueError("hare cannot move to same position")

                if next_move in current_configuration.hound_positions:
                    raise ValueError("cannot move on top of hound")

                current_configuration = Configuration(
                    next_move,
                    current_configuration.hound_positions,
                )
            else:
                move = input("move hound from to = ").split()
                if len(move) != 2:
                    raise ValueError("expected two values, `from` and `to`")

                hound_position, next_position = map(int, move)
                if hound_position not in current_configuration.hound_positions:
                    raise ValueError("invalid hound")

                if (hound_position, next_position) in BACKWARDS:
                    raise ValueError("hound cannot move backwards")

                hound = current_configuration.hound_positions.index(hound_position)

                if next_position == current_configuration.hare_position:
                    raise ValueError("cannot move on top of hare")

                if next_position in current_configuration.hound_positions:
                    raise ValueError("cannot move on top of hound")


                hpositions = current_configuration.hound_positions
                current_configuration = Configuration(
                    current_configuration.hare_position,
                    hpositions[:hound] + (next_position,) + hpositions[hound + 1:]
                )

        except ValueError as err:
            print("Invalid move:", err)
            continue
    else:
        if current_player == Player.HARE:
            next_configs = current_configuration.hare_moves()
        else:
            next_configs = current_configuration.hound_moves()

        if computer_player.is_maximizing():
            func = max
        else:
            func = min

        best_config = func(
            next_configs,
            key=lambda config: minimax(config, current_player, MAX_DEPTH)
        )

        current_configuration = best_config

    current_player = current_player.opposite()
