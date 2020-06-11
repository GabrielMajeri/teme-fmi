% Cazul de bază: indiferent de N sau de indice,
% pentru o listă vidă returnăm o listă vidă.
selectpoz([], _, _, []).

% Predicat căruia îi dau și poziția din lista inițială
% a elementului curent
selectpoz([Head|Tail], N, Indice, Output) :-
    % Calculez restul ca să văd dacă poziția
    % este multiplu de N
    Rest is Indice mod N,
    (
        (
            Rest = 0,
            Output = [Head|SubOutput]
        );
        (
            Rest \= 0,
            Output = SubOutput
        )
    ),
    % Fac i++
    UrmatorulIndice is Indice + 1,
    % Apel recursiv
    selectpoz(Tail, N, UrmatorulIndice, SubOutput).


selectpoz(Input, N, Output) :-
    % Primul element va avea indicele 1
    selectpoz(Input, N, 1, Output).
