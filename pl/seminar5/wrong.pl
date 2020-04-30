wrong_value(W, R, O, N, G, Value) :-
    Value is (W * 10000) + (R * 1000) + (O * 100) + (N * 10) + G.

right_value(R, I, G, H, T, Value) :-
    Value is (R * 10000) + (I * 1000) + (G * 100) + (H * 10) + T.

solution(W, R, O, N, G, I, H, T) :-
    wrong_value(W, R, O, N, G, WrongValue),
    right_value(R, I, G, H, T, RightValue),
    RightValue is WrongValue + WrongValue.
