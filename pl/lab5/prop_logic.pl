/* Definesc variabilele */
is_var(a).
is_var(b).
is_var(c).


/* Definesc operatorii */
:- op(630, xfx, imp).
:- op(620, xfy, si).
:- op(620, xfy, sau).
:- op(610, fy, nu).


/* Definesc ce este o formulă */
formula(X) :- is_var(X).
formula(nu X) :- formula(X).
formula(X si Y) :- formula(X), formula(Y).
formula(X sau Y) :- formula(X), formula(Y).
formula(X imp Y) :- formula(X), formula(Y).


/* Dacă formula noastră este o singură variabilă */
find_vars(Variabila, VariabileIn, VariabileOut) :-
    is_var(Variabila),
    /* Atunci o adăugăm la mulțimea variabilelor */
    union(VariabileIn, [Variabila], VariabileOut).

/* Dacă avem de a face cu negația unei formule */
find_vars(nu Formula, VariabileIn, VariabileOut) :-
    formula(Formula),
    /* Pur și simplu luăm variabilele din formulă */
    find_vars(Formula, VariabileIn, VariabileOut).

/* Dacă am de a face cu o conjuncție */
find_vars(Form1 si Form2, VIn, VOut) :-
    formula(Form1), formula(Form2),
    /* Extrag variabilele din fiecare subformulă */
    find_vars(Form1, [], V1),
    find_vars(Form2, [], V2),
    /* Reunim variabilele din cele două formule */
    union(V1, V2, V3),
    /* Le adăugăm la lista existentă de variabile */
    union(VIn, V3, VOut).

/* La fel ca mai sus */
find_vars(Form1 sau Form2, VIn, VOut) :-
    formula(Form1), formula(Form2),
    find_vars(Form1, [], V1),
    find_vars(Form2, [], V2),
    union(V1, V2, V3),
    union(VIn, V3, VOut).

/* Idem */
find_vars(Form1 imp Form2, VIn, VOut) :-
    formula(Form1), formula(Form2),
    find_vars(Form1, [], V1),
    find_vars(Form2, [], V2),
    union(V1, V2, V3),
    union(VIn, V3, VOut).

/* Generează toate șirurile binare de lungime `Nr` */
all_assigns(0, [[]]).
all_assigns(Nr, Output) :-
    Nr > 0,
    PrevNr is Nr - 1,
    all_assigns(PrevNr, Preds),
    prepend_all(0, Preds, Preds0),
    prepend_all(1, Preds, Preds1),
    append(Preds0, Preds1, Output).

/* Inserează `Value` la începutul lui `List` */
prepend(Value, List, Prepended) :- append([Value], List, Prepended).

prepend_all(_, [], []).

prepend_all(Value, [H|T], Output) :-
    prepend_all(Value, T, SubOutput),
    prepend(Value, H, Prepended),
    prepend(Prepended, SubOutput, Output).


/** Tabele de adevăr */
table_nu(0, 1).
table_nu(1, 0).

table_si(0, 0, 0).
table_si(0, 1, 0).
table_si(1, 0, 0).
table_si(1, 1, 1).

table_sau(0, 0, 0).
table_sau(0, 1, 1).
table_sau(1, 0, 1).
table_sau(1, 1, 1).

table_imp(0, 0, 1).
table_imp(0, 1, 1).
table_imp(1, 0, 0).
table_imp(1, 1, 1).

/* Găsește valoarea de adevăr a unei variabile.
 *
 * Variabila trebuie să fie element din Variables,
 * și valoarea ei se află pe poziția corespunzătoare din Assignments.
 */
/* Cazul de bază: am o singură variabilă, și aceasta are o valoare. */
truth_value(Variable, Variable, Assignment, Assignment).
/* Cazul recursiv */
truth_value(Variable, [HeadV|Vs], [HeadA|As], TruthValue) :-
    /* văd dacă variabila din capul listei este cea căutată */
    (Variable = HeadV, TruthValue = HeadA);
    /* dacă nu, continui să caut recursiv prin listă */
    (Variable \= HeadV, truth_value(Variable, Vs, As, TruthValue)).

/* Pentru celelalte operații folosesc tabelele de adevăr */
truth_value(nu Form, Vs, As, TruthValue) :-
    table_nu(FormValue, TruthValue),
    truth_value(Form, Vs, As, FormValue).

/* Ca mai sus, dar trebuie să calculez valorile celor două părți ale conjuncției */
truth_value(Form1 si Form2, Vs, As, TruthValue) :-
    table_si(Form1Value, Form2Value, TruthValue),
    truth_value(Form1, Vs, As, Form1Value),
    truth_value(Form2, Vs, As, Form2Value).

/* Idem */
truth_value(Form1 sau Form2, Vs, As, TruthValue) :-
    table_sau(Form1Value, Form2Value, TruthValue),
    truth_value(Form1, Vs, As, Form1Value),
    truth_value(Form2, Vs, As, Form2Value).

/* La fel */
truth_value(Form1 imp Form2, Vs, As, TruthValue) :-
    table_imp(Form1Value, Form2Value, TruthValue),
    truth_value(Form1, Vs, As, Form1Value),
    truth_value(Form2, Vs, As, Form2Value).

/* Cazul de bază: când am o singură atribuire de variabile */
all_values(Form, Variables, [Assignments], [TruthValue]) :-
    truth_value(Form, Variables, Assignments, TruthValue).

/* Cazul recursiv, când trebuie să trec prin listă */
all_values(Form, Variables, [HeadA|LA], Values) :-
    truth_value(Form, Variables, HeadA, HeadValue),
    append([HeadValue], SubValues, Values),
    all_values(Form, Variables, LA, SubValues).

/* Generează toate evaluarile formulei date ca parametru. */
values_all_assigns(Form, Values) :-
    /* Găsește lista variabilelor prezente în formulă */
    find_vars(Form, [], Vars),
    /* Numără câte variabile distincte avem */
    length(Vars, NumVars),
    /* Generează toate atribuirile posibile de variabile */
    all_assigns(NumVars, AllAssignments),
    /* Le testează pe toate */
    all_values(Form, Vars, AllAssignments, Values).

/* Verifică dacă o formulă este tautologie */
is_taut(Form) :-
    /* Generează toate evaluarile posibile */
    values_all_assigns(Form, Values),
    /* Verifică ca toate evaluarile să fie 1 (adevăr). */
    forall(member(TruthValue, Values), TruthValue = 1).
