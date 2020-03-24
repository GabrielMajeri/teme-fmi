% Crossword puzzle

word(abalone,a,b,a,l,o,n,e).
word(abandon,a,b,a,n,d,o,n).
word(enhance,e,n,h,a,n,c,e).
word(anagram,a,n,a,g,r,a,m).
word(connect,c,o,n,n,e,c,t).
word(elegant,e,l,e,g,a,n,t).

crossword(V1, V2, V3, H1, H2, H3) :-
    /* Folosim variabile cu nume pentru acele căsuțe care vrem să fie la fel */
    word(V1, _, A1, _, B1, _, C1, _),
    word(V2, _, A2, _, B2, _, C2, _),
    word(V3, _, A3, _, B3, _, C3, _),
    word(H1, _, A1, _, A2, _, A3, _),
    word(H2, _, B1, _, B2, _, B3, _),
    word(H3, _, C1, _, C2, _, C3, _).
