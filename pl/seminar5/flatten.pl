flatten([List], List).

flatten([H|T], Result) :-
    flatten(T, SubFlatten),
    append(H, SubFlatten, Result).
