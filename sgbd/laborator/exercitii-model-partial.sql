SET VERIFY OFF;


---- 1

-- Definesc tipurile de date pe care le returnez.
CREATE OR REPLACE TYPE t_angajat IS OBJECT (
    cod NUMBER,
    nume VARCHAR2(50),
    job_id VARCHAR2(10)
);
/

CREATE OR REPLACE TYPE t_vec_angajat IS TABLE OF t_angajat;
/


-- Definesc procedura cerută.
CREATE OR REPLACE PROCEDURE p1 (
    angajati OUT t_vec_angajat
) IS
    v_nr_max_joburi NUMBER;
BEGIN
    -- Găsesc numărul maxim de job-uri avute
    SELECT MAX(COUNT(*))
    INTO v_nr_max_joburi
    FROM job_history
    GROUP BY employee_id;
    
    -- Colectez angajații care au avut nr. maxim
    SELECT t_angajat(e.employee_id, e.last_name, e.job_id)
    BULK COLLECT INTO angajati
    FROM employees e
    INNER JOIN job_history jh
    ON e.employee_id = jh.employee_id
    GROUP BY e.employee_id, e.last_name, e.job_id
    HAVING COUNT(1) = v_nr_max_joburi;
END;
/

DECLARE
    angajati t_vec_angajat;
BEGIN
    -- Apelez procedura
    p1(angajati);
    
    -- Afișez rezultatul
    FOR i IN angajati.FIRST..angajati.LAST LOOP
        DBMS_OUTPUT.PUT_LINE('Angajatul #' || angajati(i).cod ||
            ': ' || angajati(i).nume ||
            ' cu job-ul ' || angajati(i).job_id);
    END LOOP;
END;
/


---- 2
CREATE OR REPLACE FUNCTION f2 (
    cod_manager IN NUMBER
) RETURN NUMBER
IS
    v_numar_subalterni NUMBER;
BEGIN
    SELECT COUNT(1)
    INTO v_numar_subalterni
    FROM employees
    -- Calculez recursiv toți subalternii
    START WITH employee_id = cod_manager
    CONNECT BY PRIOR employee_id = manager_id;
    
    RETURN v_numar_subalterni;
END;
/

-- Apelare într-un mod: într-un bloc anonim.
BEGIN
    DBMS_OUTPUT.PUT_LINE('Nr. de subalterni al lui 101:' || f2(101));
END;
/

-- Apelare în alt mod: în cadrul unui apel SQL obișnuit.
SELECT f2(101) AS "Nr. subalterni 101"
FROM dual;
/


---- 3
CREATE OR REPLACE PROCEDURE p3 (
    cod_angajat IN OUT NUMBER
) IS
    v_data_angajare DATE;
BEGIN
    -- Stochez data angajării.
    SELECT hire_date
    INTO v_data_angajare
    FROM employees
    WHERE employee_id = cod_angajat;
    
    -- Caut următorul angajat.
    SELECT employee_id
    INTO cod_angajat
    FROM (
        SELECT employee_id
        FROM employees
        WHERE hire_date > v_data_angajare
        ORDER BY hire_date
    )
    WHERE ROWNUM <= 1;

EXCEPTION
    -- Excepție posibilă: am primit ca parametru un angajat inexistent,
    -- sau care nu are altă persoană angajată după el
    WHEN no_data_found THEN
        RAISE_APPLICATION_ERROR(-20000,
            'Nu exista angajatul urmator');
END;
/

-- Exemplu de apelare
DECLARE
    v_cod_angajat NUMBER;
BEGIN
    v_cod_angajat := 167;
    p3(v_cod_angajat);
    DBMS_OUTPUT.PUT_LINE(v_cod_angajat);
END;
/

-- Găsește cine a fost ultima persoană angajată,
-- pentru a putea reproduce excepția în procedura de mai sus.
SELECT employee_id, first_name, last_name, hire_date
FROM (
    SELECT *
    FROM employees
    ORDER BY hire_date DESC
)
WHERE ROWNUM <= 1;
/


---- 4

-- Creez o copie a tabelului de angajați.
DROP TABLE emp;
CREATE TABLE emp AS (SELECT * FROM employees); 

ALTER TABLE emp
ADD next_sef NUMBER;
/

CREATE OR REPLACE FUNCTION gaseste_urmatorul_sef (
    cod_manager NUMBER
) RETURN NUMBER
IS
BEGIN
    RETURN 1;
END;
/

BEGIN
    DBMS_OUTPUT.PUT_LINE('Lista de viitori sefi:');
    FOR m IN (
        SELECT DISTINCT manager_id
        FROM employees
        WHERE manager_id IS NOT NULL
    )
    LOOP
        DBMS_OUTPUT.PUT_LINE('Viitorul sef al lui ' || m.manager_id ||
            ' este ' || gaseste_urmatorul_sef(m.manager_id));
    END LOOP;
END;
/
