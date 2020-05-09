1.

CREATE TABLE EMP_gma AS SELECT * FROM employees;
CREATE TABLE DEPT_gma AS SELECT * FROM departments;


2.

DESCRIBE EMP_gma;
DESCRIBE DEPT_gma;


3.

SELECT * FROM EMP_gma;
SELECT * FROM DEPT_gma;


4.

ALTER TABLE emp_gma
ADD CONSTRAINT pk_emp_gma PRIMARY KEY(employee_id);

ALTER TABLE dept_gma
ADD CONSTRAINT pk_dept_gma PRIMARY KEY(department_id);

ALTER TABLE emp_gma
ADD CONSTRAINT fk_emp_dept_gma
FOREIGN KEY(department_id) REFERENCES dept_gma(department_id);


Nu am implementat constrângerea de la un angajat la managerul lui, dacă există.


5.

SAVEPOINT StartCommit;

INSERT INTO DEPT_gma (department_id, department_name)
VALUES (300, 'Programare');

ROLLBACK TO StartCommit;


Dacă încerc să rulez de două ori comanda corectă obțin:
    ORA-00001: unique constraint (GRUPA32.PK_DEPT_GMA) violated


6.

INSERT INTO EMP_gma
VALUES (567, 'John', 'Example', 'john@example.com', null,
    sysdate,
    'IT_PROG', null, null, null, 300);


7.

INSERT INTO emp_gma (employee_id, department_id, first_name, last_name,
    email, hire_date, job_id)
VALUES (568, 300, 'Mary', 'Example',
    'mary@example.com', sysdate, 'IT_PROG');


8.

INSERT INTO emp_gma (employee_id, department_id, first_name, last_name,
    email, hire_date, job_id)
VALUES (
    (SELECT MAX(employee_id) + 1 FROM emp_gma), 300,
    'William', 'Example',
    'william@example.com', sysdate, 'IT_PROG');


9.

CREATE TABLE EMP1_gma AS (
    SELECT *
    FROM employees
    WHERE (NVL(commission_pct, 0) > 0.25)
);


10.

INSERT INTO emp_gma
VALUES (
    0,
    USER, USER,
    'TOTAL', 0, sysdate, 'TOTAL',
    (SELECT SUM(salary) FROM emp_gma),
    (SELECT AVG(NVL(commission_pct, 0)) FROM emp_gma),
    null,
    null
);


11.

UNDEFINE prenume;
UNDEFINE nume;
INSERT INTO emp_gma (employee_id, first_name, last_name, salary, email,
    job_id, hire_date)
VALUES (
    &cod,
    &&prenume,
    &&nume,
    &salariu,
    SUBSTR(&prenume, 1, 1) || SUBSTR(&nume, 1, 7) || '@example.com',
    'EXEMPLU',
    sysdate
);


12.

CREATE TABLE emp2_gma AS (SELECT * FROM employees WHERE 1=2);
CREATE TABLE emp3_gma AS (SELECT * FROM employees WHERE 1=2);

INSERT INTO emp1_gma
SELECT * FROM employees WHERE salary < 5000;

INSERT INTO emp2_gma
SELECT * FROM employees WHERE 5000 <= salary AND salary < 10000;

INSERT INTO emp3_gma
SELECT * FROM employees WHERE 10000 <= salary;

SELECT * FROM emp1_gma;
SELECT * FROM emp2_gma;
SELECT * FROM emp3_gma;

TRUNCATE TABLE emp1_gma;
TRUNCATE TABLE emp2_gma;
TRUNCATE TABLE emp3_gma;


13.

CREATE TABLE emp0_gma AS (SELECT * FROM employees WHERE 1=2);


INSERT INTO emp0_gma
SELECT * FROM employees WHERE department_id = 80;

INSERT INTO emp1_gma
SELECT * FROM employees
WHERE (salary < 5000) AND department_id != 80;

INSERT INTO emp2_gma
SELECT * FROM employees
WHERE ((5000 <= salary) AND (salary < 10000))
    AND (department_id != 80);

INSERT INTO emp2_gma
SELECT * FROM employees
WHERE (10000 <= salary) AND (department_id != 80);


14.

SELECT * FROM emp_gma;

UPDATE emp_gma
SET salary = 1.05 * salary;

SELECT * FROM emp_gma;

ROLLBACK;


15.

UPDATE emp_gma
SET job_id = 'SA_REP'
WHERE department_id = 80 AND commission_pct IS NOT null;

SELECT * FROM emp_gma;

ROLLBACK;


16.

UPDATE dept_gma
SET manager_id = (
    SELECT employee_id
    FROM emp_gma
    WHERE first_name LIKE 'Douglas' AND last_name LIKE 'Grant'
)
WHERE department_id = 20;

UPDATE emp_gma
SET salary = salary + 1000
WHERE first_name LIKE 'Douglas' AND last_name LIKE 'Grant';

ROLLBACK;


17.

UPDATE emp_gma
SET (salary, commission_pct) = (
    SELECT salary, commission_pct
    FROM emp_gma
    WHERE employee_id = (
        SELECT manager_id
        FROM employees
        WHERE salary = (
            SELECT MIN(salary)
            FROM employees
        )
    )
)
WHERE employee_id = (
    SELECT employee_id
    FROM employees
    WHERE salary = (
        SELECT MIN(salary)
        FROM employees
    )
);

ROLLBACK;


18.

UPDATE emp_gma
SET email = SUBSTR(last_name, 1, 1) || NVL(first_name, '.')
WHERE salary = (
    SELECT MAX(salary)
    FROM employees
    WHERE employees.department_id = emp_gma.department_id
);

ROLLBACK;


19.

UPDATE emp_gma
SET salary = (SELECT AVG(salary) FROM emp_gma)
WHERE hire_date = (
    SELECT MIN(hire_date)
    FROM employees
    WHERE employees.department_id = emp_gma.department_id
);

ROLLBACK;


20.

UPDATE emp_gma
SET (job_id, department_id) = (
    SELECT job_id, department_id
    FROM employees
    WHERE employee_id = 205
)
WHERE employee_id = 114;

ROLLBACK;


21.

SELECT *
FROM dept_gma
WHERE department_id = &&id_departament;

UPDATE dept_gma
SET department_name = &nume,
    manager_id = &manager,
    location_id = &locatie
WHERE department_id = &id_departament;

ROLLBACK;


22.

DELETE FROM dept_gma;

ROLLBACK;


23.

DELETE FROM emp_gma
WHERE commission_pct IS NULL;

ROLLBACK;


24.

DELETE FROM dept_gma d
WHERE NOT EXISTS (
    SELECT 1
    FROM employees e
    WHERE d.department_id = e.department_id
);

ROLLBACK;


25.

SELECT *
FROM emp_gma
WHERE employee_id = &&cod_angajat;

DELETE FROM emp_gma
WHERE employee_id = &cod_angajat;

ROLLBACK;


26.


27.

SAVEPOINT;


28.

DELETE FROM emp_gma;
SELECT * FROM emp_gma;


29.

ROLLBACK;


30.

SELECT * FROM emp_gma;

COMMIT;


31.

DELETE FROM emp_gma
WHERE commission_pct IS NOT null;

INSERT ALL INTO emp_gma
SELECT * FROM employees
WHERE commission_pct IS NOT null;

ROLLBACK;
