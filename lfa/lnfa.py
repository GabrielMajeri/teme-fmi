#!/usr/bin/env python3

LAMBDA_SYMBOL = '.'

# Read the input data
with open('input.txt') as file:
    # Read the whole file into memory as an array of lines
    lines = file.readlines()

    # Strip the trailing newline from every line
    lines = map(str.strip, lines)

    state_count = int(next(lines))
    states = next(lines).split()

    alphabet_size = int(next(lines))
    alphabet = next(lines).split()

    initial_state = next(lines)

    final_state_count = int(next(lines))
    final_states = set(next(lines).split())

    # Read the transitions as a dictionary of transitions
    transition_count = int(next(lines))
    transitions = {initial_state:{} for initial_state in states}
    for _ in range(transition_count):
        initial, symbol, final = next(lines).split()
        if symbol in transitions[initial]:
            transitions[initial][symbol].append(final)
        else:
            transitions[initial][symbol] = [final]

    word_count = int(next(lines))
    words = list(lines)

def lambda_closure(state):
    """Computes the set of all states accesible from a given starting state
    reachable using only lambda transitions."""

    # Use a breadth-first traversal of the state graph
    visited = {state:False for state in states}
    queue = [state]
    closure = set()

    while queue:
        node = queue.pop(0)
        closure.add(node)

        # Determine which lambda-transitions are possible
        for destination in transitions[node].get(LAMBDA_SYMBOL, ()):
            if not visited[destination]:
                queue.append(destination)

        # Mark this state as visited
        visited[node] = True

    return closure

def check_word(word):
    """Checks that a given word is accepted or rejected by the LNFA."""

    # This set contains all the possible states in which we currently are
    current_states = lambda_closure(initial_state)

    # Go through each character in the word
    for symbol in word:
        new_states = set()

        # Determine which states we can reach
        for start_state in current_states:
            if symbol in transitions[start_state]:
                possible_states = transitions[start_state][symbol]

                for state in possible_states:
                    new_states |= lambda_closure(state)

        # No more possible states to checks
        if not new_states:
            return False

        current_states = new_states

    # See if we reached a final state in any way
    return bool(current_states & final_states)

with open('output.txt', 'w') as file:
    for result in map(check_word, words):
        print("DA" if result else "NU", file=file)
