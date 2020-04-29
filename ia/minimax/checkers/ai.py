from math import inf

INFINITY = inf

def best_move(config, player, max_depth, prune):
    "Finds the best move starting from a given configuration."
    next_configs = config.next_configurations(player)

    minimax_fn = alpha_beta if prune else minimax

    opposite_player = player.opposite()

    scores = {}
    for next_config in next_configs:
        # for every possible move, compute a score
        if next_config.capture_chain:
            next_player = player
        else:
            next_player = opposite_player

        score = minimax_fn(next_config, next_player, max_depth)
        scores[next_config] = score

    if player.is_maximizing():
        optimum_func = max
    else:
        optimum_func = min

    return optimum_func(next_configs, key=lambda cfg: scores[cfg])

def minimax(config, player, depth):
    if depth == 0:
        return config.score()

    next_configs = config.next_configurations(player)

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

def alpha_beta(config, player, depth, alpha=-INFINITY, beta=+INFINITY):
    if depth == 0:
        return config.score()

    next_configs = config.next_configurations(player)

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

        next_score = alpha_beta(next_config, next_player, depth - 1, alpha, beta)

        if player.is_maximizing():
            score = max(score, next_score)
            alpha = max(alpha, score)
        else:
            score = min(score, next_score)
            beta = min(beta, score)

        if alpha >= beta:
            break

    return score
