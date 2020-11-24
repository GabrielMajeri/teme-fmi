SET VERIFY OFF;

--- Exerciții
-- 1

DROP TABLE info;

CREATE TABLE info (
    utilizator NVARCHAR2(30),
    data DATE,
    comanda NVARCHAR2(50),
    nr_linii NUMBER,
    eroare NVARCHAR2(100)
);

COMMIT;

SELECT *
FROM info;

/


-- 2
CREATE OR REPLACE FUNCTION f2 (
    v_nume employees.last_name%TYPE DEFAULT 'Bell'
) RETURN NUMBER IS
    salariu employees.salary%TYPE;
BEGIN
    SELECT
        salary
    INTO salariu
    FROM
        employees
    WHERE
        last_name = v_nume;
        
    -- S-a găsit cu succes un singur angajat
    INSERT INTO info VALUES (USER, SYSDATE, 'F2', 1, NULL);
    COMMIT;

    RETURN salariu;
EXCEPTION
    WHEN no_data_found THEN
        INSERT INTO info VALUES (USER, SYSDATE, 'F2', 0, 'Nu exista angajatul');
        COMMIT;
        
        raise_application_error(-20000, 'Nu exista angajati cu numele dat');
    WHEN too_many_rows THEN
        INSERT INTO info VALUES (USER, SYSDATE, 'F2', 0, 'Exista mai multi angajati cu numele dat');
        COMMIT;
        
        raise_application_error(-20001, 'Exista mai multi angajati cu numele dat');
    WHEN OTHERS THEN
        INSERT INTO info VALUES (USER, SYSDATE, 'F2', 0, 'Alta eroare');
        COMMIT;
        
        raise_application_error(-20002, 'Alta eroare!');
END f2;
/

DECLARE
    v_salary NUMBER;
BEGIN
    v_salary := f2('Cambrault');
END;
/


CREATE OR REPLACE PROCEDURE p4 (
    v_nume employees.last_name%TYPE
) IS
    salariu employees.salary%TYPE;
BEGIN
    SELECT
        salary
    INTO salariu
    FROM
        employees
    WHERE
        last_name = v_nume;

    dbms_output.put_line('Salariul este ' || salariu);
    
    INSERT INTO info VALUES (USER, SYSDATE, 'F2', 1, NULL);
    COMMIT;
EXCEPTION
    WHEN no_data_found THEN
        INSERT INTO info VALUES (USER, SYSDATE, 'F2', 0, 'Nu exista angajatul');
        COMMIT;
        
        raise_application_error(-20000, 'Nu exista angajati cu numele dat');
    WHEN too_many_rows THEN
        INSERT INTO info VALUES (USER, SYSDATE, 'F2', 0, 'Exista mai multi angajati cu numele dat');
        COMMIT;
        
        raise_application_error(-20001, 'Exista mai multi angajati cu numele dat');
    WHEN OTHERS THEN
        INSERT INTO info VALUES (USER, SYSDATE, 'F2', 0, 'Alta eroare');
        COMMIT;
        
        raise_application_error(-20002, 'Alta eroare!');
END p4;
/

BEGIN
    p4(NULL);
END;
/


-- 4
CREATE OR REPLACE PROCEDURE e4 (
    v_manager_id employees.manager_id%TYPE
) IS
    TYPE t_ids IS TABLE OF employees.employee_id%TYPE;
    v_ids t_ids;
BEGIN
    -- Selectez toți angajații care lucrează direct sau indirect
    -- sub managerul dat.
    SELECT employee_id
    BULK COLLECT INTO v_ids
    FROM employees
    START WITH employee_id = v_manager_id
    CONNECT BY manager_id = PRIOR employee_id;
    
    -- Cazul când primesc un cod care nu este al unui manager.
    IF v_ids.COUNT = 0 THEN
        DBMS_OUTPUT.PUT_LINE('Nu există un manager cu codul dat');
        RETURN;
    END IF;
    
    -- Măresc salariile
    FOR i IN v_ids.FIRST..v_ids.LAST
    LOOP
        UPDATE employees
        SET salary = salary * 1.1
        WHERE employee_id = v_ids(i);
    END LOOP;
END;
/


BEGIN
    -- 100 este King, o să crească salariul tuturor angajațiilor
    e4(100);
END;
/

SELECT salary
FROM employees;

ROLLBACK;
