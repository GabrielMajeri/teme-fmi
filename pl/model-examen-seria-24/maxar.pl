% Cazul de bază: lista vidă nu are termeni
maxar_lista([], 0, []).

% Predicat recursiv care găsește termenii de aritate maximă
% dintr-o listă
maxar_lista([Head|Tail], AritateMaxima, Output) :-
    % Calculăm aritatea maximă a primului termen
    maxar(Head, HeadArMax, HeadOutput),
    % Căutăm maximele în restul listei
    maxar_lista(Tail, SubArMaxima, SubOutput),
    % Returnăm maximul
    (
        (
            HeadArMax > SubArMaxima,
            AritateMaxima = HeadArMax,
            Output = HeadOutput
        );
        (
            HeadArMax = SubArMaxima,
            AritateMaxima = HeadArMax,
            append(HeadOutput, SubOutput, Output)
        );
        (
            HeadArMax < SubArMaxima,
            AritateMaxima = SubArMaxima,
            Output = SubOutput
        )
    ).

% Dacă termenul este doar o variabilă, o lăsăm așa
maxar(Variabila, Aritate, Output) :-
    var(Variabila),
    Aritate = 0,
    Output = [Variabila].

maxar(Termen, AritateMaxima, Output) :-
    % Variabilele sunt tratate mai sus
    nonvar(Termen),
    % Descompunem termenul
    Termen =.. [Operator|Parametrii],
    % Extragem aritatea
    length(Parametrii, Aritate),
    % Vedem ce arități maxime găsim între parametrii
    maxar_lista(Parametrii, ArMaxParam, OutputParam),
    % Returnăm maximul
    (
        (
            Aritate > ArMaxParam,
            AritateMaxima = Aritate,
            OutputList = [Operator]
        );
        (
            Aritate = ArMaxParam,
            AritateMaxima = Aritate,
            OutputList = [Operator|OutputParam]
        );
        (
            Aritate < ArMaxParam,
            AritateMaxima = ArMaxParam,
            OutputList = OutputParam
        )
    ),
    % E posibil să avem același operator de aritate maximă
    % care să se repete, din cerință pare că nu vrea
    % să afișăm duplicatele.
    list_to_set(OutputList, Output).
