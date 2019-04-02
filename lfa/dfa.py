#!/usr/bin/env python3

input_file = 'exemplu.txt'
output_file = 'output.txt'

# Read the input data
with open(input_file, 'r') as f:
    # Read the whole file line-by-line
    lines = f.readlines()

    # Remove newlines from the input
    lines = map(str.strip, lines)

    # Read the labels of the states
    state_count = int(next(lines))
    states = next(lines).split()

    # Read the alphabet
    alphabet_size = int(next(lines))
    alphabet = next(lines).split()

    # Determine the initial state
    initial_state = next(lines)

    # Read a list of final states
    final_state_count = int(next(lines))
    final_states = next(lines).split()

    # Build the transition function as a dictionary-of-dictionaries
    transition_count = int(next(lines))
    transitions = {state:{} for state in states}
    for i in range(transition_count):
        start, letter, end = next(lines).split()
        transitions[start][letter] = end

    # The remaining lines are the words to be checked
    word_count = int(next(lines))
    words = list(lines)

def check_word(word):
    """Check that the given word is accepted by the DFA."""

    state = initial_state

    for character in word:
        # Determine which transitions are valid from the current state
        possible_transitions = transitions[state]

        # Determine if we can transition to another state
        if character in possible_transitions:
            state = possible_transitions[character]
        else:
            # No more valid transitions are available
            return False

    # We've gone through the whole word, see what kind of state we are in
    if state in final_states:
        # If we end up in a final state
        return True
    else:
        # Otherwise we are in a valid, but not final state
        return False

# Check all the words for matches
with open(output_file, 'w') as f:
    for word in words:
        accepted = check_word(word)
        message = "DA" if accepted else "NU"

        #print(f"{word}: {message}")
        print(message, file=f)
