---- Laboratorul 6: triggers

---- Exerciții
---- 1
-- Fac o copie a tabelului `departments`
DROP TABLE dept;
CREATE TABLE dept AS (SELECT * FROM departments);

-- Creez un trigger care să nu-i permită decât utilizatorului „Scott”
-- să șteargă departamente.
CREATE OR REPLACE TRIGGER ex1
BEFORE DELETE ON dept
FOR EACH ROW
BEGIN
    IF USER != 'SCOTT' THEN
        RAISE_APPLICATION_ERROR(-20000, 'Utilizatorul nu este Scott');
    END IF;
END;
/

-- Dacă încerc să execut această linie cu utilizatorul meu,
-- voi primi o eroare:
DELETE FROM dept;
/

---- 2
-- Fac o copie a tabelului `employees`
DROP TABLE emp;
CREATE TABLE emp AS (SELECT * FROM employees);

CREATE OR REPLACE TRIGGER ex2
BEFORE UPDATE OF commission_pct ON emp
FOR EACH ROW
BEGIN
    -- Dacă comisionul depășește 50%, nu permit actualizarea.
    IF :NEW.commission_pct > 0.5 THEN
        RAISE_APPLICATION_ERROR(-20000, 'Nu se poate mari comisionul');
    END IF;
END;
/

-- Încerc să pun un comision prea mare.
UPDATE emp
SET commission_pct = 0.76
WHERE employee_id = 100;
/


---- 3
-- (Re)creez tabelul de angajați
DROP TABLE info_emp;
CREATE TABLE info_emp AS (SELECT * FROM employees);

-- (Re)creez tabelul de departamente
DROP TABLE info_dept;
CREATE TABLE info_dept AS (SELECT * FROM departments);

-- Adaug o coloană pentru numărul de angajați din acel departament
ALTER TABLE info_dept
ADD numar NUMBER;

-- O inițializez
UPDATE info_dept d
SET numar = (
    SELECT COUNT(1)
    FROM info_emp
    WHERE department_id = d.department_id
);

-- Mă asigur că nu poate fi setată ca NULL pe viitor
ALTER TABLE info_dept
MODIFY numar NUMBER NOT NULL;

-- Creez un trigger care să o păstreze actualizată
CREATE OR REPLACE TRIGGER ex3
AFTER INSERT OR UPDATE OR DELETE ON info_emp
BEGIN
    -- Rulez din nou codul de mai sus
    UPDATE info_dept d
    SET numar = (
        SELECT COUNT(1)
        FROM info_emp
        WHERE department_id = d.department_id
    );
END;
/


---- 4
-- Creez un tabel în care să rețin câți angajați am în fiecare departament
DROP TABLE num_emps;
CREATE TABLE num_emps AS (
    SELECT department_id, COUNT(1) AS num_emp
    FROM emp
    GROUP BY department_id
);

-- Creez un trigger care (re)inițializează tabelul auxiliar
-- la fiecare insert sau update.
CREATE OR REPLACE TRIGGER ex4_init
BEFORE INSERT OR UPDATE ON emp
BEGIN
    -- Șterg datele vechi
    DELETE FROM num_emps;

    -- Inserez valorile actuale
    INSERT INTO num_emps (department_id, num_emp)
    SELECT department_id, COUNT(1)
    FROM emp
    GROUP BY department_id;
END;
/


CREATE OR REPLACE TRIGGER ex4
BEFORE INSERT OR UPDATE ON emp
FOR EACH ROW
DECLARE
    v_num_ang NUMBER;
BEGIN
    -- Văd câți angajați am în acest moment în departament
    SELECT num_emp INTO v_num_ang
    FROM num_emps
    WHERE department_id = :NEW.department_id;

    -- Dacă depășesc limita
    IF v_num_ang >= 45 THEN
        RAISE_APPLICATION_ERROR(-20000, 'Prea multi angajati');
    END IF;

    -- Contorizăm și acest angajat
    UPDATE num_emps
    SET num_emp = num_emp + 1
    WHERE department_id = :NEW.department_id;
END;
/

-- Văd ce departamente sunt deja pline
SELECT department_id, COUNT(*) AS num_emps
FROM emp
GROUP BY department_id
HAVING COUNT(*) >= 45;

-- Nu o să mă lase: prea mulți angajați
INSERT ALL
    INTO emp (employee_id, department_id)
    VALUES (1234, 50)
    INTO emp (employee_id, department_id)
    VALUES (1235, 50)
SELECT 1 FROM dual;

-- Testez un insert multiplu
INSERT INTO emp
SELECT * FROM employees WHERE department_id = 50;
ROLLBACK;


---- 5
DROP TABLE emp_test;

CREATE TABLE emp_test AS (
    SELECT employee_id, last_name, first_name, department_id
    FROM employees
);

ALTER TABLE emp_test
ADD CONSTRAINT emp_test_pk PRIMARY KEY (employee_id);

CREATE TABLE dept_test AS (
    SELECT department_id, department_name
    FROM departments
);

ALTER TABLE dept_test
ADD CONSTRAINT dept_test_pk PRIMARY KEY (department_id);

-- Comenzi de  afișare
SELECT * FROM emp_test;
SELECT * FROM dept_test;

CREATE OR REPLACE TRIGGER ex5_stergere
AFTER DELETE ON dept_test
FOR EACH ROW
BEGIN
    -- Șterg angajații care erau în acest departament
    DELETE FROM emp_test
    WHERE department_id = :OLD.department_id;
END;
/

CREATE OR REPLACE TRIGGER ex5_modificare
AFTER UPDATE ON dept_test
FOR EACH ROW
BEGIN
    -- Modific angajații care erau deja în departament.
    UPDATE emp_test
    SET department_id = :NEW.department_id
    WHERE department_id = :OLD.department_id;
END;
/

-- Scenarii posibile
ALTER TABLE emp_test
DROP CONSTRAINT emp_test_dept_fk;

ALTER TABLE emp_test
ADD CONSTRAINT emp_test_dept_fk
    FOREIGN KEY (department_id)
    REFERENCES dept_test(department_id);

ALTER TABLE emp_test
ADD CONSTRAINT emp_test_dept_fk
    FOREIGN KEY (department_id)
    REFERENCES dept_test(department_id)
    ON DELETE CASCADE;

ALTER TABLE emp_test
ADD CONSTRAINT emp_test_dept_fk
    FOREIGN KEY (department_id)
    REFERENCES dept_test(department_id)
    ON DELETE SET NULL;


---- 6
-- (Re)creez tabelul în care să rețin erorile.
DROP TABLE database_errors;
CREATE TABLE database_errors (
    user_id NVARCHAR2(100),
    nume_bd NVARCHAR2(100),
    erori NVARCHAR2(2000),
    data DATE
);

-- Adaug trigger-ul care salvează erorile.
-- Nu funcționează deoarece nu am destule permisiuni
CREATE OR REPLACE TRIGGER ex6
AFTER SERVERERROR
ON DATABASE
BEGIN
    INSERT INTO database_errors
    VALUES (
        SYS.LOGIN_USER,
        SYS.DATABASE_NAME,
        DBMS.FORMAT_ERROR_STACK,
        SYSDATE
    );
END;


---- Extra
-- Să se creeze un trigger check_sal_*** care garantează ca, ori de câte ori un angajat nou este
-- introdus în tabelul EMP_*** sau atunci când este modificat salariul sau codul job-ului unui
-- angajat, salariul se încadrează între minimul úi maximul salariior corespunzătoare job-ului
-- respectiv. Se vor exclude angajatii AD_PRES.

-- Tabel auxiliar în care rețin valorile minime și maxime ale salariilor per fiecare job.
CREATE TABLE emp_min_max AS (
    SELECT
        job_id,
        MIN(salary) AS min_salary,
        MAX(salary) AS max_salary
    FROM emp
    GROUP BY job_id
);

CREATE OR REPLACE TRIGGER check_sal_1
BEFORE UPDATE OF salary ON emp
BEGIN
    UPDATE emp_min_max e
    SET min_salary = (SELECT MIN(salary) FROM emp WHERE job_id = e.job_id),
        max_salary = (SELECT MAX(salary) FROM emp WHERE job_id = e.job_id);
END;
/

CREATE OR REPLACE TRIGGER check_sal_2
BEFORE UPDATE OF salary ON emp
FOR EACH ROW
WHEN (NEW.job_id != 'AD_PRES')
DECLARE
    job_min_salary NUMBER;
    job_max_salary NUMBER;
BEGIN
    SELECT min_salary, max_salary
    INTO job_min_salary, job_max_salary
    FROM emp_min_max
    WHERE job_id = :NEW.job_id;

    IF (:NEW.salary < job_min_salary) OR (:NEW.salary > job_max_salary) THEN
        RAISE_APPLICATION_ERROR(-20000, 'Nu este permisa modificarea');
    END IF;
END;
/

SELECT min_salary, max_salary
FROM emp_min_max
WHERE job_id = (SELECT job_id FROM emp WHERE employee_id = 120);

UPDATE emp
SET salary = 5801
WHERE employee_id = 120;
