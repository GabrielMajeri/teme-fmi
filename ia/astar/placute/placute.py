from collections import defaultdict
from copy import deepcopy
from heapq import heappush, heappop
from math import inf
import time

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

def cell_count_heuristic(board):
    "Counts how many non-empty cells the board has."
    count = 0
    for line in board:
        for cell in line:
            if cell != EMPTY_SYMBOL:
                count += 1

    return count

def zone_count_heuristic(board):
    """Counts how many colors are there left.

    Admissible because we have to use a move for at least each color.
    """
    colors = set()
    for zone_color, _ in get_zones(board):
        colors.add(zone_color)
    return len(colors)

def fragmentation_heuristic(board):
    """Estimates how fragmented the board is.

    This works because we'd get the same total cost if
    we would remove each zone individually.
    """
    zones = get_zones(board)
    num_zones = len(zones)

    if num_zones == 0:
        return 0

    color_counts = defaultdict(int)
    for zone_color, zone in zones:
        color_counts[zone_color] += len(zone)

    cost = 0
    for zone_color, zone in zones:
        cost += 1 - len(zone)/color_counts[zone_color]

    return cost

heuristic = None


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
        return list(reversed(path))

def astar(initial_board, output_path=None):
    start_time = time.perf_counter()

    initial_node = Node(initial_board, distance=0)

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

    end_time = time.perf_counter()

    total_time = end_time - start_time

    print(f"A* finished in {num_steps} steps = {total_time:.2f} seconds")

    if current_node.is_goal():
        print("Found solution")
        path = current_node.recreate_path()

        total_cost = 0
        for node in path[1:]:
            cost = node.distance - node.predecessor.distance
            total_cost += cost
        print(f"Total cost: {total_cost:.2f}")

        if output_path:
            with open(output_path, "w") as fout:
                print(path[0], file=fout)
                print(file=fout)

                for node in path[1:]:
                    cost = node.distance - node.predecessor.distance

                    print(node, file=fout)
                    print(f"Cost: {cost:.2f}", file=fout)
                    print(file=fout)

    else:
        print("Not solvable")

def main():
    for index in range(1, 5):
        print(f"\tInput #{index}")

        input_path = f'input_{index}.txt'
        output_path = f'output_{index}.txt'

        initial_board = read_input(input_path)

        global heuristic
        print("Using cell count heuristic (not admissible)")
        heuristic = cell_count_heuristic
        astar(initial_board)
        print()

        print("Using zone count heuristic")
        heuristic = zone_count_heuristic
        astar(initial_board)
        print()

        print("Using fragmentation heuristic")
        heuristic = fragmentation_heuristic
        astar(initial_board, output_path)
        print()


if __name__ == '__main__':
    main()
