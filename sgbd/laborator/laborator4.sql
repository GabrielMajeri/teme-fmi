SET SERVEROUTPUT ON;
SET VERIFY OFF;

--- Exerciții
-- 1

-- Copiem tabelul pentru a nu-l afecta pe cel original.
DROP TABLE emp_copy;
CREATE TABLE emp_copy AS (SELECT * FROM employees);
COMMIT;

DECLARE
    -- Colecție care reține ID-ul angajaților care nu câștigă comision
    TYPE emp_ids_vector IS VARRAY(5)
        OF emp_copy.employee_id%TYPE NOT NULL;
    emps emp_ids_vector := emp_ids_vector();
    emp_id emp_copy.employee_id%TYPE;
    emp_salary emp_copy.salary%TYPE;
BEGIN
    SELECT *
    BULK COLLECT INTO emps
    FROM
    (
        SELECT employee_id
        FROM emp_copy
        WHERE commission_pct IS NULL
        ORDER BY salary ASC
    )
    WHERE rownum <= 5;

    FOR i IN emps.FIRST..emps.LAST LOOP
        emp_id := emps(i);

        DBMS_OUTPUT.PUT_LINE('Actualizez salariatul #' || emp_id);

        SELECT salary
        INTO emp_salary
        FROM emp_copy
        WHERE employee_id = emp_id;

        DBMS_OUTPUT.PUT_LINE('  Salariu vechi: ' || emp_salary);

        UPDATE emp_copy
        SET salary = salary * 1.05
        WHERE employee_id = emp_id;

        SELECT salary
        INTO emp_salary
        FROM emp_copy
        WHERE employee_id = emp_id;

        DBMS_OUTPUT.PUT_LINE('  Salariu nou: ' || emp_salary);
    END LOOP;
END;
/


-- 2
-- Implementare cu VARRAY

-- Creez tabelul care reține excursiile
DROP TABLE excursie;

CREATE OR REPLACE TYPE tip_orase AS VARRAY(50) OF VARCHAR2(10);
/

CREATE TABLE excursie (
    cod_excursie NUMBER(4) PRIMARY KEY,
    denumire VARCHAR2(20) UNIQUE NOT NULL,
    orase tip_orase NOT NULL,
    status VARCHAR2(20) DEFAULT 'disponibila' NOT NULL
);

-- Inserez date
DELETE FROM excursie;
INSERT ALL
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (1, 'Ex1', tip_orase('Bucuresti', 'Sinaia', 'Brasov'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (2, 'Ex2', tip_orase('Constanta', 'Mangalia'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (3, 'Alta Excursie', tip_orase('Bucuresti', 'Brasov', 'Cluj', 'Arad'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (4, 'Test', tip_orase('Constanta'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (5, 'Un nume', tip_orase('Paris', 'Londra', 'New York', 'Chicago'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (10, 'MiniExcursie', tip_orase('Madrid'))
SELECT * FROM dual;

COMMIT;
/

-- Afișez datele
SELECT * FROM excursie;
/

DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Adaug orașul nou la final
    v_orase.EXTEND;
    v_orase(v_orase.COUNT) := &oras;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/


-- Adaugă un oraș după primul element
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Adaug orașul după primul element din vector.
    v_orase.EXTEND;
    FOR i IN REVERSE v_orase.NEXT(2)..v_orase.LAST LOOP
        v_orase(i) := v_orase(i - 1);
    END LOOP;
    v_orase(2) := &oras;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/


-- Interschimbă două orașe
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
    v_index1 NUMBER;
    v_index2 NUMBER;
    v_oras_aux VARCHAR(20);
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Caut orașele prin vector
    FOR i IN v_orase.FIRST..v_orase.LAST LOOP
        IF v_orase(i) = &nume_oras1 THEN
            v_index1 := i;
        ELSIF v_orase(i) = &nume_oras2 THEN
            v_index2 := i;
        END IF;
    END LOOP;

    -- Interschimb
    v_oras_aux := v_orase(v_index1);
    v_orase(v_index1) := v_orase(v_index2);
    v_orase(v_index2) := v_oras_aux;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/


-- Șterge un oraș după nume
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
    v_index NUMBER := NULL;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Caut orașul în vector
    FOR i IN v_orase.FIRST..v_orase.LAST LOOP
        IF v_orase(i) = &nume_oras THEN
            v_index := i;
        END IF;
    END LOOP;

    -- Șterg orașul
    FOR i IN (v_index + 1)..v_orase.COUNT LOOP
        v_orase(i - 1) := v_orase(i);
    END LOOP;

    v_orase.TRIM;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/



-- Afișează detalii despre orașele unei excursii
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    DBMS_OUTPUT.PUT_LINE('Nr. orase: ' || v_orase.COUNT);

    FOR i IN v_orase.FIRST..v_orase.LAST LOOP
        DBMS_OUTPUT.PUT_LINE('Oras #' || i || ': ' || v_orase(i));
    END LOOP;
END;
/


-- Afișează lista de orașe pentru fiecare excursie, pe rând
DECLARE
    TYPE tip_coduri_excursii IS TABLE OF excursie.cod_excursie%TYPE;
    v_coduri_excursii tip_coduri_excursii;
    v_orase tip_orase;
BEGIN
    FOR e IN (SELECT cod_excursie, orase FROM excursie) LOOP
        DBMS_OUTPUT.PUT_LINE('Excursie #' || e.cod_excursie);

        FOR j IN 1..(e.orase.COUNT) LOOP
            DBMS_OUTPUT.PUT_LINE('  Oras #' || j || ': ' || e.orase(j));
        END LOOP;
    END LOOP;
END;
/


-- Anulează excursiile cu cele mai puține orașe vizitate
DECLARE
    v_nr_min NUMBER := 99999;
BEGIN
    FOR e IN (SELECT orase FROM excursie) LOOP
        IF e.orase.COUNT < v_nr_min THEN
            v_nr_min := e.orase.COUNT;
        END IF;
    END LOOP;

    FOR e IN (SELECT cod_excursie, orase FROM excursie) LOOP
        -- Dacă are număr minim de orașe
        IF e.orase.COUNT = v_nr_min THEN
            -- Atunci anulez excursia
            UPDATE excursie
            SET status = 'anulata'
            WHERE cod_excursie = e.cod_excursie;
        END IF;
    END LOOP;
END;
/

SELECT * FROM excursie;
/


-- 3
-- Implementare cu tabele imbricate

-- Creez tabelul care reține excursiile
DROP TABLE excursie;

CREATE OR REPLACE TYPE tip_orase AS TABLE OF VARCHAR2(10);
/

CREATE TABLE excursie (
    cod_excursie NUMBER(4) PRIMARY KEY,
    denumire VARCHAR2(20) UNIQUE NOT NULL,
    orase tip_orase,
    status VARCHAR2(20) DEFAULT 'disponibila' NOT NULL
)
NESTED TABLE orase STORE AS excursie_orase;

-- Inserez date
DELETE FROM excursie;
INSERT ALL
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (1, 'Ex1', tip_orase('Bucuresti', 'Sinaia', 'Brasov'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (2, 'Ex2', tip_orase('Constanta', 'Mangalia'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (3, 'Alta Excursie', tip_orase('Bucuresti', 'Brasov', 'Cluj', 'Arad'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (4, 'Test', tip_orase('Constanta'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (5, 'Un nume', tip_orase('Paris', 'Londra', 'New York', 'Chicago'))
    INTO excursie (cod_excursie, denumire, orase)
    VALUES (10, 'MiniExcursie', tip_orase('Madrid'))
SELECT * FROM dual;

COMMIT;
/

-- Afișez datele
SELECT * FROM excursie;
/

DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Adaug orașul nou la final
    v_orase.EXTEND;
    v_orase(v_orase.COUNT) := &oras;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/


-- Adaugă un oraș după primul element
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Adaug orașul după primul element din vector.
    v_orase.EXTEND;
    FOR i IN REVERSE v_orase.NEXT(2)..v_orase.LAST LOOP
        v_orase(i) := v_orase(i - 1);
    END LOOP;
    v_orase(2) := &oras;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/


-- Interschimbă două orașe
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
    v_index1 NUMBER;
    v_index2 NUMBER;
    v_oras_aux VARCHAR(20);
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Caut orașele prin vector
    FOR i IN v_orase.FIRST..v_orase.LAST LOOP
        IF v_orase(i) = &nume_oras1 THEN
            v_index1 := i;
        ELSIF v_orase(i) = &nume_oras2 THEN
            v_index2 := i;
        END IF;
    END LOOP;

    -- Interschimb
    v_oras_aux := v_orase(v_index1);
    v_orase(v_index1) := v_orase(v_index2);
    v_orase(v_index2) := v_oras_aux;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/


-- Șterge un oraș după nume
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
    v_index NUMBER := NULL;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    -- Caut orașul în vector
    FOR i IN v_orase.FIRST..v_orase.LAST LOOP
        IF v_orase(i) = &nume_oras THEN
            v_index := i;
        END IF;
    END LOOP;

    -- Șterg orașul
    FOR i IN (v_index + 1)..v_orase.COUNT LOOP
        v_orase(i - 1) := v_orase(i);
    END LOOP;

    v_orase.TRIM;

    UPDATE excursie
    SET orase = v_orase
    WHERE cod_excursie = v_cod_excursie;
END;
/

SELECT * FROM excursie;
/



-- Afișează detalii despre orașele unei excursii
DECLARE
    v_cod_excursie CONSTANT NUMBER NOT NULL := &cod;
    v_orase tip_orase;
BEGIN
    SELECT orase INTO v_orase
    FROM excursie
    WHERE cod_excursie = v_cod_excursie;

    DBMS_OUTPUT.PUT_LINE('Nr. orase: ' || v_orase.COUNT);

    FOR i IN 1..v_orase.COUNT LOOP
        DBMS_OUTPUT.PUT_LINE('Oras #' || i || ': ' || v_orase(i));
    END LOOP;
END;
/


-- Afișează lista de orașe pentru fiecare excursie, pe rând
DECLARE
    TYPE tip_coduri_excursii IS TABLE OF excursie.cod_excursie%TYPE;
    v_coduri_excursii tip_coduri_excursii;
    v_orase tip_orase;
BEGIN
    FOR e IN (SELECT cod_excursie, orase FROM excursie) LOOP
        DBMS_OUTPUT.PUT_LINE('Excursie #' || e.cod_excursie);

        FOR j IN 1..(e.orase.COUNT) LOOP
            DBMS_OUTPUT.PUT_LINE('  Oras #' || j || ': ' || e.orase(j));
        END LOOP;
    END LOOP;
END;
/


-- Anulează excursiile cu cele mai puține orașe vizitate
DECLARE
    v_nr_min NUMBER := 99999;
BEGIN
    FOR e IN (SELECT orase FROM excursie) LOOP
        IF e.orase.COUNT < v_nr_min THEN
            v_nr_min := e.orase.COUNT;
        END IF;
    END LOOP;

    FOR e IN (SELECT cod_excursie, orase FROM excursie) LOOP
        -- Dacă are număr minim de orașe
        IF e.orase.COUNT = v_nr_min THEN
            -- Atunci anulez excursia
            UPDATE excursie
            SET status = 'anulata'
            WHERE cod_excursie = e.cod_excursie;
        END IF;
    END LOOP;
END;
/

SELECT * FROM excursie;
/
