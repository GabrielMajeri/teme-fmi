1.

CREATE GLOBAL TEMPORARY TABLE temp_trans_gma
    (x NUMBER);

INSERT INTO temp_trans_gma
VALUES (3.1415);

SELECT * FROM temp_trans_gma;

COMMIT;


2.

CREATE GLOBAL TEMPORARY TABLE temp_sesiune_gma
    (x NUMBER)
    ON COMMIT PRESERVE ROWS;

INSERT INTO temp_sesiune_gma
VALUES (3.1415);

SELECT * FROM temp_sesiune_gma;

COMMIT;


3.

DESC temp_trans_gma;
DESC temp_sesiune_gma;

SELECT *  FROM temp_trans_gma;
SELECT *  FROM temp_sesiune_gma;


4.

TRUNCATE TABLE temp_trans_gma;
TRUNCATE TABLE temp_sesiune_gma;


5.

CREATE GLOBAL TEMPORARY TABLE angajati_azi_gma
  (first_name VARCHAR(50), last_name VARCHAR2(50), cod_ang NUMBER(3));


6.

INSERT INTO angajati_azi_gma
VALUES ('Ion', 'Popescu', 123);

ALTER TABLE angajati_azi_gma
MODIFY (last_name VARCHAR2(30));


7.

CREATE VIEW viz_emp30_gma
AS (
    SELECT cod_ang, nume, email, salariu
    FROM angajati_gma
    WHERE cod_dep = 30
);

SELECT * FROM viz_emp30_gma;

INSERT INTO viz_emp30_gma
VALUES (130, 'NumeAng', 'email_ang', 5000);

SELECT * FROM angajati_gma;

ROLLBACK;


8.

UPDATE viz_emp30_gma
SET email = email || '@dept30.com';

ROLLBACK;


9.

CREATE VIEW viz_empsal50_gma AS (
    SELECT
        cod_ang AS cod_angajat,
        nume,
        email,
        job AS functie,
        data_ang AS data_angajare,
        salariu * 12 AS sal_anual
    FROM angajati_gma
    WHERE cod_dep = 50
);

SELECT * FROM viz_empsal50_gma;


10.

INSERT INTO viz_empsal50_gma
COLUMNS (cod_angajat, nume, email, functie)
VALUES (150, 'Test', 'exemplu@gmail.com', 'Angajat');


11.

a)
DESC viz_emp30_gma;
DESC departamente_gma;

CREATE VIEW viz_emp_dep30_gma AS (
    SELECT
        cod_ang,
        viz_emp30_gma.nume AS nume,
        departamente_gma.nume AS nume_dep,
        cod_dep
    FROM viz_emp30_gma, departamente_gma
    WHERE cod_dep = 30
);

b)
INSERT INTO viz_emp_dep30_gma
COLUMNS (cod_ang, nume, nume_dep)
VALUES (200, 'Nume', 'Departamentul Exemplu');


12.

DROP VIEW viz_dept_sum_gma;
CREATE VIEW viz_dept_sum_gma AS (
    SELECT
        cod_dep,
        MIN(salariu) AS min_sal,
        MAX(salariu) AS max_sal,
        AVG(salariu) AS medie_sal
    FROM departamente_gma
    INNER JOIN angajati_gma
    USING (cod_dep)
    GROUP BY cod_dep
);

SELECT * FROM viz_dept_sum_gma;

Nu se poate actualiza nicio coloană.


13.

CREATE OR REPLACE VIEW viz_emp30_gma
AS
    SELECT cod_ang, nume, email, salariu, cod_dep
    FROM angajati_gma
    WHERE cod_dep = 30 WITH CHECK OPTION CONSTRAINT viz_check_dept30_gma;

SELECT * FROM user_constraints
WHERE LOWER(constraint_name) = 'viz_check_dept30_gma';

INSERT INTO viz_emp30_gma
VALUES (300, 'Exemplu', 'email', 15000, 31);


14.

CREATE VIEW viz_emp_s_gma AS
    SELECT *
    FROM angajati_gma
    WHERE (
        SELECT nume
        FROM departamente_gma
        WHERE cod_dep = angajati_gma.cod_dep
    ) LIKE 'S%';

SELECT * FROM departamente_gma;
SELECT * FROM viz_emp_s_gma;


15.

SELECT * FROM user_views;


16.

SELECT *
FROM angajati_gma
INNER JOIN viz_dept_sum_gma
USING (cod_dep);


17.

CREATE VIEW viz_sal_gma AS
    SELECT last_name, department_name, salary, city
    FROM employees
    INNER JOIN departments
    USING (department_id)
    INNER JOIN locations
    USING (location_id);

SELECT * FROM viz_sal_gma;


18.

CREATE VIEW viz_emp_gma (
    employee_id,
    first_name,
    last_name,
    email UNIQUE DISABLE NOVALIDATE,
    phone_number,
    CONSTRAINT pk_viz_emp_gma PRIMARY KEY (employee_id) DISABLE NOVALIDATE
) AS
    SELECT employee_id, first_name, last_name, email, phone_number
    FROM employees;

SELECT * FROM viz_emp_gma;


19.

ALTER VIEW viz_emp_s_gma
ADD PRIMARY KEY (cod_ang) DISABLE NOVALIDATE;


20.

CREATE SEQUENCE seq_dept_gma
START WITH 200
INCREMENT BY 10
MAXVALUE 10000
NOCYCLE NOCACHE;


21.

SELECT * FROM user_sequences;


22.

CREATE SEQUENCE seq_emp_gma;


23.

DROP TABLE emp_gma;
CREATE TABLE emp_gma AS (SELECT * FROM employees);

UPDATE emp_gma
SET employee_id = seq_emp_gma.nextval;

SELECT * FROM emp_gma;


24.

INSERT INTO emp_gma
VALUES (
    seq_emp_gma.nextval,
    'Ion', 'Popescu',
    'email',
    null,
    '10-JUN-2000',
    'FI_ACCOUNT',
    5000,
    null,
    null,
    100
);

INSERT INTO dept_gma
VALUES (seq_dept_gma.nextval, 'Department', null, 3000);


25.

SELECT seq_emp_gma.currval FROM dual;
SELECT seq_dept_gma.currval FROM dual;


26.

DROP SEQUENCE seq_dept_gma;


27.

CREATE INDEX idx_emp_last_name_gma
    ON emp_gma (last_name);


28.

CREATE UNIQUE INDEX idx_emp_id_gma
    ON emp_gma (employee_id);

ALTER TABLE emp_gma
ADD PRIMARY KEY (employee_id)
ADD UNIQUE (last_name, first_name, hire_date);


29.

CREATE INDEX idx_dept_id_gma
    ON emp_gma (department_id);


30.

CREATE INDEX idx_dept_name_gma
    ON dept_gma (UPPER(department_name));

CREATE INDEX idx_emp_name_gma
    ON emp_gma (LOWER(last_name));


31.

SELECT index_name, column_name, column_position, uniqueness
FROM user_indexes
INNER JOIN user_ind_columns
USING (index_name)
WHERE LOWER(user_indexes.table_name) IN ('emp_gma', 'dept_gma');


32.

DROP INDEX idx_emp_last_name_gma;


33.

CREATE CLUSTER angajati_gma (angajat NUMBER(6))
SIZE 512
STORAGE (initial 100 next 50);


34.

CREATE INDEX idx_angajati_gma ON CLUSTER angajati_gma;


35.

CREATE TABLE ang_1_gma
CLUSTER angajati_gma(employee_id)
AS SELECT * FROM employees WHERE salary < 5000;

CREATE TABLE ang_2_gma
CLUSTER angajati_gma(employee_id)
AS SELECT * FROM employees WHERE 5000 <= salary AND salary < 10000;

CREATE TABLE ang_3_gma
CLUSTER angajati_gma(employee_id)
AS SELECT * FROM employees WHERE salary >= 10000;


36.

SELECT * FROM user_clusters;


37.

SELECT cluster_name
FROM user_tables
WHERE LOWER(table_name) = 'ang_3_gma';


38.

DROP TABLE ang_3_gma;


39.

SELECT * FROM user_tables
WHERE LOWER(table_name) = 'ang_3_gma';


40.

DROP TABLE ang_2_gma;

SELECT * FROM user_tables
WHERE LOWER(cluster_name) = 'angajati_gma';


41.

DROP CLUSTER angajati_gma
INCLUDING TABLES
CASCADE CONSTRAINTS;


42.

CREATE PUBLIC SYNONYM emp_public_gma FOR emp_gma;


43.

CREATE SYNONYM v30_gma FOR viz_emp30_gma;


44.

CREATE SYNONYM dept_syn_gma FOR dept_gma;

SELECT * FROM dept_syn_gma;

RENAME dept_gma TO dept_redenumit_gma;

RENAME dept_redenumit_gma TO dept_gma;


45.

SELECT * FROM user_synonyms
WHERE LOWER(synonym_name) LIKE '%gma';

DROP SYNONYM v30_gma;
DROP SYNONYM dept_syn_gma;


46.

CREATE MATERIALIZED VIEW job_dep_sal_gma
BUILD IMMEDIATE
REFRESH COMPLETE
ENABLE QUERY REWRITE
AS SELECT d.department_name, j.job_title, SUM(salary) AS suma_salarii
FROM employees e
INNER JOIN departments d
ON e.department_id = d.department_id
INNER JOIN jobs j
ON e.job_id = j.job_id
GROUP BY d.department_name, j.job_title;


47.

CREATE TABLE job_dep_gma (
    job VARCHAR2 (10),
    dep NUMBER (4),
    suma_salarii NUMBER(9, 2)
);

CREATE MATERIALIZED VIEW job_dep_gma
ON PREBUILT TABLE WITH REDUCED PRECISION
ENABLE QUERY REWRITE AS (
    SELECT j.job_title, d.department_id, SUM(salary) AS suma_salarii
    FROM employees e
    INNER JOIN departments d
    ON e.department_id = d.department_id
    INNER JOIN jobs j
    ON e.job_id = j.job_id
    GROUP BY d.department_id, j.job_title
);


48.

CREATE MATERIALIZED VIEW LOG ON dept_gma;

CREATE MATERIALIZED VIEW dep_vm_gma
REFRESH FAST START WITH SYSDATE NEXT SYSDATE + 1/288
WITH PRIMARY KEY
AS SELECT * FROM dept_gma;


49.

ALTER MATERIALIZED VIEW job_dep_sal_gma
REFRESH FAST NEXT SYSDATE + 7 DISABLE QUERY REWRITE;


50.

DROP MATERIALIZED VIEW dep_vm_gma;
DROP MATERIALIZED VIEW job_dep_sal_gma;
