/* Implementarea unui limbaj imperativ în Prolog */

/*** Sintactica ***/

/* Definesc operatorii */
:- op(100, xf, {}).
:- op(1100, yf, ;).

/* Definesc categoriile sintactice */

/* O expresie este
 *   fie un număr întreg
 *   fie o variabilă (un atom Prolog)
 */
arithmetic_expression(I) :- integer(I).
arithmetic_expression(X) :- atom(X).

/*   fie suma a două subexpresii */
arithmetic_expression(A1 + A2) :-
    arithmetic_expression(A1),
    arithmetic_expression(A2).

/*   fie diferența a două subexpresii */
arithmetic_expression(A1 - A2) :-
    arithmetic_expression(A1),
    arithmetic_expression(A2).

/*   fie produsul a două subexpresii */
arithmetic_expression(A1 * A2) :-
    arithmetic_expression(A1),
    arithmetic_expression(A2).

/* Constantele true și false sunt expresii boolene */
boolean_expression(true).
boolean_expression(false).

/* Operatorii pe expresii boolene */
boolean_expression(and(BE1, BE2)) :-
    boolean_expression(BE1),
    boolean_expression(BE2).

boolean_expression(or(BE1, BE2)) :-
    boolean_expression(BE1),
    boolean_expression(BE2).

boolean_expression(not(BE)) :-
    boolean_expression(BE).

/* Operatorii de comparație */
boolean_expression(A1 =< A2) :-
    arithmetic_expression(A1), arithmetic_expression(A2).
boolean_expression(A1 >= A2) :-
    arithmetic_expression(A1), arithmetic_expression(A2).
boolean_expression(A1 == A2) :-
    arithmetic_expression(A1), arithmetic_expression(A2).

/* Instrucțiunile */

/* Instrucțiunea care nu face nimic */
statement(skip).
/* Atribuirea unei variabile */
statement(X = AE) :-
    atom(X),
    arithmetic_expression(AE).

/* Înlănțuire de instrucțiuni */
statement(Stmt1; Stmt2) :-
    statement(Stmt1), statement(Stmt2).

/* Instrucțiuni între paranteze */
statement( (Stmt1; Stmt2) ) :-
    statement(Stmt1), statement(Stmt2).

/* Bloc de instrucțiuni */
statement( {Stmt} ) :- statement(Stmt).

/* Instrucțiune if */
statement(if(BE, Stmt1, Stmt2)) :-
    boolean_expression(BE),
    statement(Stmt1), statement(Stmt2).

/* While loop */
statement(while(BE, Stmt)) :-
    boolean_expression(BE), statement(Stmt).


/* Definiția unui program */
program(Stmt, TargetExpr) :-
    /* Programul trebuie să fie valid sintactic */
    statement(Stmt),
    /* Expresia a cărei valoare vrem să o determinăm */
    arithmetic_expression(TargetExpr).

test0 :- program({
        x = 10;
        sum = 0;
        while(0 =< x, {
            sum = sum + x;
            x = x - 1
        })
    }, sum).

/*** Semantica ***/

/* Extragem valoarea unei variabile din State */
get(State, Variabila, Valoare) :- member(vi(Variabila, Valoare), State).
/* Dacă variabila nu există, returnăm 0 */
get(_, _, 0).

/* Ștergem o variabilă din state */
del([vi(Variable, _)|State], Variable, State).
del([H|SubState1], Variable, [H|SubState2]) :- del(SubState1, Variable, SubState2).
del([], _, []).

/* Setăm valoarea unei variabile */
set(State, Variable, Value, [vi(Variable, Value)|SubState]) :-
    del(State, Variable, SubState).

/* Citirea unei variabile */
smallstepA(Variable, State, Value, State) :-
    atom(Variable),
    get(State, Variable, Value).


/* Adunarea a două constante întregi */
smallstepA(Immediate1 + Immediate2, State, Result, State) :-
    integer(Immediate1), integer(Immediate2),
    Result is Immediate1 + Immediate2.

/* Adunarea unei constante cu o expresie */
smallstepA(Immediate + Expr1, State, Immediate + Expr2, State) :-
    integer(Immediate),
    smallstepA(Expr1, State, Expr2, State).

/* Adunarea a două expresii */
smallstepA(Expr1 + Expr, State, Expr2 + Expr, State) :-
    smallstepA(Expr1, State, Expr2, State).


/* Scăderea a două constante întregi */
smallstepA(Immediate1 - Immediate2, State, Result, State) :-
    integer(Immediate1), integer(Immediate2),
    Result is Immediate1 - Immediate2.

/* Scăderea unei constante cu o expresie */
smallstepA(Immediate - Expr1, State, Immediate - Expr2, State) :-
    integer(Immediate),
    smallstepA(Expr1, State, Expr2, State).

/* Scăderea a două expresii */
smallstepA(Expr1 - Expr, State, Expr2 - Expr, State) :-
    smallstepA(Expr1, State, Expr2, State).


/* Înmulțirea a două constante întregi */
smallstepA(Immediate1 * Immediate2, State, Result, State) :-
    integer(Immediate1), integer(Immediate2),
    Result is Immediate1 * Immediate2.

/* Înmulțirea unei constante cu o expresie */
smallstepA(Immediate * Expr1, State, Immediate * Expr2, State) :-
    integer(Immediate),
    smallstepA(Expr1, State, Expr2, State).

/* Înmulțirea a două expresii */
smallstepA(Expr1 * Expr, State, Expr2 * Expr, State) :-
    smallstepA(Expr1, State, Expr2, State).


smallstepB(I1 =< I2, State, true, State) :-
    integer(I1), integer(I2),
    (I1 =< I2).

smallstepB(I1 =< I2, State, false, State) :-
    integer(I1), integer(I2),
    (I1 > I2).

smallstepB(I =< AE1, State, I =< AE2, State) :-
    integer(I),
    smallstepA(AE1, State, AE2, State).

smallstepB(AE1 =< AE, State, AE2 =< AE, State) :-
    smallstepA(AE1, State, AE2, State).

smallstepB(not(true), State, false, State).
smallstepB(not(false), State, true, State).

smallstepB(not(BE1), State, not(BE2), State) :-
    smallstepB(BE1, State, BE2, State).

smallstepS({E}, State, E, State).
smallstepS((skip; Stmt2), State, Stmt2, State).
smallstepS((Stmt1; Stmt), State, (Stmt2; Stmt), NewState) :-
    smallstepS(Stmt1, State, Stmt2, NewState) .

smallstepS(X = AE, State, skip, NewState) :-
    integer(AE),
    set(State, X, AE, NewState).

smallstepS(X = AE1, State, X = AE2, State) :-
    smallstepA(AE1, State, AE2, State).

smallstepS(if(true, Stmt1, _), State, Stmt1, State).
smallstepS(if(false, _, Stmt2), State, Stmt2, State).
smallstepS(if(BE1, Stmt1, Stmt2), State, if(BE2, Stmt1, Stmt2), State) :-
    smallstepB(BE1, State, BE2, State).

smallstepS(while(BE, Stmt), State, if(BE, (Stmt; while(BE, Stmt)), skip), State).


smallstepP(skip, AE1, S1, skip, AE2, S2) :-
    smallstepA(AE1, S1, AE2, S2).

smallstepP(St1, AE, S1, St2, AE, S2) :-
    smallstepS(St1, S1, St2, S2).

run(skip, I, _, I) :- integer(I).
run(St1, AE1, S1, I) :-
    smallstepP(St1, AE1, S1, St2, AE2, S2),
    run(St2, AE2, S2, I).

run_program(Name) :-
    defpg(Name, {P}, E),
    run(P,E, [],I),
    write(I).

defpg(pg2,
    {
        x = 10;
        sum = 0;
        while(0 =< x, {
            sum = sum + x;
            x = x - 1
        })
    }, sum).

mytrace(skip, I, _) :- integer(I).
mytrace(St1, AE1, S1) :-
    smallstepP(St1, AE1, S1, St2, AE2, S2),
    write(St2), nl,
    write(AE2), nl,
    write(S2), nl,
    mytrace(St2, AE2, S2).

trace_program(Name) :-
    defpg(Name, {Program}, Expr),
    mytrace(Program, Expr, []).
