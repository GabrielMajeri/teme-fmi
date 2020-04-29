from collections import defaultdict
from copy import deepcopy
from heapq import heappush, heappop
from math import inf


EMPTY_SYMBOL = '#'
INFINITY = +inf

def read_input(path):
    with open(path) as fin:
        board = []
        for line in fin:
            row = list(line.strip())
            board.append(row)
        return board

def get_zones(board):
    rows = len(board)

    if rows == 0:
        return []

    columns = len(board[0])

    visited = [[False for _ in range(columns)] for _ in range(rows)]

    def visit(i, j):
        zone = [(i, j)]

        if ((i > 0) and (not visited[i - 1][j]) and
                (board[i][j] == board[i - 1][j])):
            visited[i - 1][j] = True
            zone += visit(i - 1, j)

        if ((i < rows - 1) and (not visited[i + 1][j]) and
                (board[i][j] == board[i + 1][j])):
            visited[i + 1][j] = True
            zone += visit(i + 1, j)

        if ((j > 0) and (not visited[i][j - 1]) and
                (board[i][j] == board[i][j - 1])):
            visited[i][j - 1] = True
            zone += visit(i, j - 1)

        if ((j < columns - 1) and (not visited[i][j + 1]) and
                (board[i][j] == board[i][j + 1])):
            visited[i][j + 1] = True
            zone += visit(i, j + 1)

        return zone

    zones = []

    for column in range(columns):
        for row in range(rows):
            if visited[row][column]:
                continue

            visited[row][column] = True

            zone = visit(row, column)

            zone_color = board[row][column]

            if zone_color == EMPTY_SYMBOL:
                continue

            zones.append((zone_color, zone))

    return zones


def remove_zone(board, zone):
    for row, column in zone:
        board[row][column] = EMPTY_SYMBOL

    rows = len(board)
    columns = len(board[0])

    column = 0
    while column < columns:
        # shift down blocks
        row = rows - 1
        while row > 0:
            if board[row][column] == EMPTY_SYMBOL:
                search_row = row - 1
                while search_row >= 0 and board[search_row][column] == EMPTY_SYMBOL:
                    search_row -= 1

                # we finished this column
                if search_row < 0:
                    break

                board[row][column] = board[search_row][column]
                board[search_row][column] = EMPTY_SYMBOL

            row -= 1

        # delete empty columns
        if board[rows - 1][column] == EMPTY_SYMBOL:
            for row in range(rows):
                del board[row][column]
            columns -= 1
        else:
            column += 1

    # delete empty rows
    row = 0
    while row < rows:
        empty = all(map(lambda cell: cell == EMPTY_SYMBOL, board[row]))
        if empty:
            del board[row]
            rows -= 1
        else:
            row += 1


def trivial_heuristic(_board):
    "Trivial heuristic, which is the same for every node."
    return 0

def zone_count_heuristic(board):
    "Counts how many contiguous zones there are still left."
    return len(get_zones(board))

def cell_count_heuristic(board):
    "Counts how many non-empty cells the board has."
    count = 0
    for line in board:
        for cell in line:
            if cell != EMPTY_SYMBOL:
                count += 1

    return count

# Modify this variable to change the chosen heuristic
heuristic = cell_count_heuristic


class Node:
    "Node in the A* search graph"

    def __init__(self, board, distance, predecessor=None):
        self.board = board

        self.distance = distance
        self.heuristic_value = heuristic(board)

        # Precompute the node's value
        self.value = self.distance + self.heuristic_value

        # Save the predecessor for retracing the path at the end
        self.predecessor = predecessor

    def __repr__(self):
        return '\n'.join(''.join(row) for row in self.board)

    def __lt__(self, other):
        return self.value < other.value

    def is_goal(self):
        "Predicate which checks if this node is the target node."
        return len(self.board) == 0

    def get_successors(self):
        "Returns the successors of this node in the traversal."
        successors = []
        zones = get_zones(self.board)

        zone_sizes = defaultdict(int)
        for zone_color, zone in zones:
            zone_sizes[zone_color] += len(zone)

        for zone_color, zone in zones:
            zone_size = len(zone)

            # only zones with at least three blocks can be popped
            if zone_size < 3:
                continue

            new_board = deepcopy(self.board)
            remove_zone(new_board, zone)

            cost = 2 - zone_size / zone_sizes[zone_color]
            node = Node(new_board, self.distance + cost, predecessor=self)

            successors.append(node)

        return successors

    def recreate_path(self):
        "Starting from a node, recreates the path to the root of the traversal tree."
        node = self
        path = [node]
        while node.predecessor:
            node = node.predecessor
            path.append(node)
        return reversed(path)

def main():
    input_path = 'example_input.txt'

    board = read_input(input_path)

    initial_node = Node(board, distance=0)

    open_nodes = [initial_node]
    minimum_distances = defaultdict(lambda: INFINITY)

    num_steps = 0
    while open_nodes:
        current_node = heappop(open_nodes)

        if current_node.is_goal():
            break

        for next_node in current_node.get_successors():
            if next_node.distance < minimum_distances[next_node]:
                minimum_distances[next_node] = next_node.distance
                heappush(open_nodes, next_node)

        num_steps += 1

    print("A* finished in", num_steps, "steps")

    if current_node.is_goal():
        print("Found solution")

        path = current_node.recreate_path()

        print(*path, sep='\n\n')
    else:
        print("Not solvable")


if __name__ == '__main__':
    main()
