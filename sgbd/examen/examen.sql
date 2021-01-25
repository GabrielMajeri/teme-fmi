---- Subiectul I

-- Am creat o schemă simplă pentru a reține tipurile de pizza
-- care sunt oferite de o pizzerie

CREATE TABLE ingredient (
    id NUMBER
        CONSTRAINT ingredient_pk PRIMARY KEY,
    nume NVARCHAR2(50) NOT NULL
);

CREATE TABLE pizza (
    id NUMBER
        CONSTRAINT pizza_pk PRIMARY KEY,
    nume NVARCHAR2(150) NOT NULL
);

-- Tabel pentru many-to-many între pizza și ingrediente
CREATE TABLE contine (
    id_pizza NUMBER
        CONSTRAINT contine_pizza_fk REFERENCES pizza(id),
    id_ingredient NUMBER
        CONSTRAINT contine_ingredient_fk REFERENCES ingredient(id),

    -- Cantitatea folosită, în grame.
    -- Poate fi NULL, adică se pune o cantitate variabilă/după gust
    cantitate NUMBER,

    -- Cheia primară este compusă
    CONSTRAINT contine_pk PRIMARY KEY (id_pizza, id_ingredient)
);


-- Populare tabele
INSERT INTO ingredient VALUES (1, 'Mozarella');
INSERT INTO ingredient VALUES (2, 'Sos de rosii');
INSERT INTO ingredient VALUES (3, 'Masline');
INSERT INTO ingredient VALUES (4, 'Pepperoni');
INSERT INTO ingredient VALUES (5, 'Porumb');

INSERT INTO pizza VALUES (1, 'Marguerita');
INSERT INTO pizza VALUES (2, 'Vegetariana');
INSERT INTO pizza VALUES (3, 'Diavola');

INSERT INTO contine VALUES (1, 1, 750);
INSERT INTO contine VALUES (1, 2, 300);
INSERT INTO contine VALUES (1, 3, 150);

INSERT INTO contine VALUES (2, 1, 1000);
INSERT INTO contine VALUES (2, 2, 300);
INSERT INTO contine VALUES (2, 3, 200);
INSERT INTO contine VALUES (2, 5, 150);

INSERT INTO contine VALUES (3, 1, 650);
INSERT INTO contine VALUES (3, 2, 100);
INSERT INTO contine VALUES (3, 4, 500);


---- Subiectul II

/*
a) O cheie primară reprezintă o coloană (sau mai multe coloane),
prin care se poate identifica în mod unic un rând din tabel.
În exemplul meu, `pizza.id` identifică în mod unic fiecare tip de pizza.
Sistemul se asigură că fiecare rând are o valoare nenulă pentru cheia primară,
și că nu există duplicate.

O cheie externă este o coloană (sau mai multe) care fac referire la
cheia primară a altui tabel, identificând un rând din acel tabel.
În exemplul meu, `contine.id_ingredient` se referă la un rând din tabelul
`ingredient`. Sistemul se asigură că, atunci când inserez sau actualizez
acea coloană, să facă referire la un ingredient valid.
*/

/*
b) Asemănări între vector și tablou imbricat:
- Atât un vector cât și un tablou imbricat pot fi folosiți și în afara PL/SQL
  (când definim tabele în SQL simplu, de exemplu).
- Atât vectorii cât și tabelele trebuie să fie extinși înainte de a insera
  un element nou.

Diferențe:
- Un vector este întotdeauna dens, datele sunt contigue în memorie.
  Un tablou imbricat este dens la început, dar poate ajunge să aibă și spații goale.
- Un vector are o capacitate definită la început, care nu poate fi depășită.
  Un tablou imbricat poate crește să fie oricât de mare este nevoie, în limita
  constrângerilor de spațiu/memorie.
*/

/*
c) Un cursor este un obiect PL/SQL care conține o referință la
o mulțime de rânduri (întoarse de o clauză SQL), permițând iterarea prin aceasta.

Deosebiri între cursoare predefinite și cursoare dinamice:
- Un cursor predefinit folosește întotdeauna aceeași cerere. Un cursor dinamic
poate fi (re)deschis cu cereri diferite.
- Un cursor dinamic poate fi transmis ca parametru și returnat de subprograme.
*/

/*
d) Pizzeria are și un regim de happy hour în fiecare zi de la 10 la 14,
în care toate pizzele au aceeași rețetă, dar cantitatea ingredientelor crește de 1.5 ori.

Se cere să se definească un view care să returneze noile liste de ingrediente
asupra cărora s-a aplicat modificarea de happy hour. Noua cantitate
se va calcula folosind un subprogram, pentru a facilita modificările ulterioare.

Trebuie să folosim o funcție, ca să putem returna direct noua cantitate.
*/

CREATE OR REPLACE FUNCTION cantitate_happy_hour(
    cantitate NUMBER
) RETURN NUMBER
IS
BEGIN
    IF cantitate IS NULL THEN
        RETURN NULL;
    END IF;

    IF (TO_CHAR(SYSDATE,'HH24') NOT BETWEEN 10 AND 14) THEN
        RETURN cantitate * 1.5;
    ELSE
        RETURN cantitate;
    END IF;
END;
/

CREATE OR REPLACE VIEW contine_happy_hour AS
SELECT id_pizza, id_ingredient, cantitate_happy_hour(cantitate) AS cantitate
FROM contine;

SELECT * FROM contine_happy_hour;

/*
e) Asemănări între trigger la nivel de linie și trigger la nivel de tabel:
- Amândouă tipurile de trigger pot fi definite pentru orice tip de comandă LMD
  (insert, update și delete)
- Este necesară aceeași permisiune la nivel de schemă pentru a crea triggeri
  la nivel de linie, respectiv tabel.

Deosebiri între trigger la nivel de linie și trigger la nivel de tabel:
- Un trigger la nivel de tabel se execută indiferent câte rânduri ajung să fie modificate.
  Un trigger la nivel de linie se execută pentru fiecare linie modificată,
  sau chiar deloc dacă nu există astfel de linii.
- Un trigger la nivel de linie are acces la variabilele OLD/NEW pentru a accesa
  valoarea precedentă/nouă a rândului curent. Într-un trigger la nivel de tabel
  nu există asemenea variabile.
*/


---- Subiectul III
--- Exercițiul 1.
-- Am ales relația one-to-many între `pizza` și `contine`


CREATE OR REPLACE TYPE t_contine AS OBJECT (
    id_ingredient NUMBER,
    cantitate NUMBER
);
/

-- Vector pentru o listă de ingrediente
CREATE OR REPLACE TYPE t_vector AS VARRAY(50) OF t_contine;
/
-- Tabel pentru o listă de ingrediente
CREATE OR REPLACE TYPE t_tabel AS TABLE of t_contine;
/

-- a)
CREATE TABLE r1 (
    id_pizza NUMBER PRIMARY KEY
        CONSTRAINT r1_pizza_fk REFERENCES pizza(id),
    reteta t_vector
);

CREATE TABLE r2 (
    id_pizza NUMBER PRIMARY KEY
        CONSTRAINT r2_pizza_fk REFERENCES pizza(id),
    reteta t_tabel
) NESTED TABLE reteta STORE AS r2_reteta;


-- b)
CREATE OR REPLACE PROCEDURE insert_r1 (id NUMBER)
IS
    v_reteta t_vector;
BEGIN
    -- Salvez într-un vector lista de ingrediente
    SELECT t_contine(id_ingredient, cantitate)
    BULK COLLECT INTO v_reteta
    FROM contine
    WHERE contine.id_pizza = id;

    -- Inserez în r1
    INSERT INTO r1
    VALUES (id, v_reteta);
END;
/


-- Testez subprogramul
BEGIN
    insert_r1(1);
    insert_r1(3);
END;
/

SELECT * FROM r1;

-- c)
CREATE OR REPLACE PROCEDURE copy_r1_to_r2
IS
    v_tabel t_tabel;
BEGIN
    FOR i IN (SELECT * FROM r1) LOOP
        -- Convertesc vectorul într-un tabel sortat
        SELECT t_contine(id_ingredient, cantitate)
        BULK COLLECT INTO v_tabel
        FROM TABLE(i.reteta)
        ORDER BY id_ingredient;

        -- Inserez rândul în r2
        INSERT INTO r2
        VALUES (i.id_pizza, v_tabel);
    END LOOP;
END;
/

-- Testez
BEGIN
    copy_r1_to_r2;
END;
/

-- d)
DECLARE
    v_id_pizza NUMBER;
    v_tip_tabel NUMBER;

    v_vector t_vector;
    v_tabel t_tabel;

    v_cursor SYS_REFCURSOR;
    v_id_ingredient NUMBER;
BEGIN
    v_id_pizza := &id_pizza;
    v_tip_tabel := &tip_tabel;

    IF v_tip_tabel = 1 THEN
        SELECT reteta INTO v_vector
        FROM r1
        WHERE id_pizza = v_id_pizza;

        OPEN v_cursor FOR
        SELECT id_ingredient
        FROM TABLE(v_vector);
    ELSIF v_tip_tabel = 2 THEN
        SELECT reteta INTO v_tabel
        FROM r2
        WHERE id_pizza = v_id_pizza;

        OPEN v_cursor FOR
        SELECT id_ingredient
        FROM TABLE(v_tabel);
    ELSE
        raise_application_error(-20000,
            'Tipul tabelului poate fi 1 sau 2');
    END IF;

    DBMS_OUTPUT.PUT('Ingrediente: ');
    LOOP
        FETCH v_cursor INTO v_id_ingredient;
        EXIT WHEN v_cursor%NOTFOUND;

        DBMS_OUTPUT.PUT(v_id_ingredient || ' ');
    END LOOP;

    DBMS_OUTPUT.PUT_LINE('');
END;
/


-- Exercițiul 2
-- Elimin cheia primară existentă pe tabelul `ingredient`
ALTER TABLE ingredient
DROP CONSTRAINT ingredient_pk;

CREATE OR REPLACE TRIGGER simulare_ingredient_pk
BEFORE INSERT OR UPDATE OF id
ON ingredient
FOR EACH ROW
DECLARE
    v_num_acelasi_id NUMBER;
BEGIN
    IF :NEW.id IS NULL THEN
        raise_application_error(-20000,
            'ID nu poate fi null');
    END IF;

    SELECT COUNT(1)
    INTO v_num_acelasi_id
    FROM ingredient
    WHERE id = :NEW.id;

    IF v_num_acelasi_id > 0 THEN
        raise_application_error(-20000,
            'Exista deja alt rand cu acest ID');
    END IF;
END;
/

INSERT INTO ingredient VALUES (1, 'Test');
INSERT INTO ingredient VALUES (NULL, 'Test');
INSERT INTO ingredient VALUES (15, 'Test');
