Exercițiul 1

SELECT CONCAT(first_name, CONCAT(' ', last_name))
    || ' castiga ' || salary
    || ' lunar dar doreste ' || (3 * salary) "Salariu ideal"
FROM employees;


Exercițiul 2

SELECT INITCAP(first_name) Prenume, UPPER(last_name) Nume, LENGTH(last_name) "Lungime nume"
FROM employees
WHERE
    first_name LIKE 'J%' OR first_name LIKE 'M%'
    OR SUBSTR(first_name, 2, 1) = 'a'
ORDER BY "Lungime nume" DESC;


Exercițiul 3

SELECT employee_id, last_name, department_id
FROM employees
WHERE LOWER(first_name) LIKE '%steven%';


Exercițiul 4

SELECT employee_id, last_name, LENGTH(last_name) "Lungime nume", 
    INSTR(LOWER(last_name), 'a') "Prima poziție a lui 'a'"
FROM employees
WHERE last_name NOT LIKE '%e';


Exercițiul 5

SELECT first_name, last_name
FROM employees
WHERE
    MOD(TRUNC(SYSDATE - hire_date), 7) = 0;


Exercițiul 6

SELECT employee_id, last_name, salary,
    ROUND(salary * 1.15, 2) "Salariu nou",
    MOD(FLOOR(ROUND(salary * 1.15, 2) / 100), 10) "Numar sute"
FROM employees
WHERE MOD(salary, 1000) != 0;


Exercițiul 7

SELECT first_name || ' ' || last_name "Nume angajat",
    RPAD(TO_CHAR(hire_date, 'DD/MM/YYYY'), 14) "Data angajarii"
FROM employees
WHERE commission_pct != 0;


Exercițiul 8

SELECT SYSDATE + 30 "Peste 30 de zile"
FROM dual;


Exercițiul 9

SELECT ROUND(LAST_DAY('01-DEC-2020') - SYSDATE) "Numar zile"
FROM dual;


Exercițiul 10

SELECT TO_CHAR(SYSDATE, 'HH24:MI DD/MM/YYYY')
FROM dual;

SELECT TO_CHAR((SYSDATE + 0.5), 'HH24:MI DD/MM/YYYY') "Peste 12 ore"
FROM dual;

SELECT TO_CHAR(SYSDATE + 5 * (1/(24 * 60)), 'HH24:MI DD/MM/YYYY') "Peste 5 minute"
FROM dual;


Exercițiul 11

SELECT last_name || ' ' || first_name "Nume",
    hire_date "Data angajari",
    NEXT_DAY(ADD_MONTHS(hire_date, 6), 'Monday') "Zi negociere salariu"
FROM employees;


Exercițiul 12

SELECT last_name, ROUND(MONTHS_BETWEEN(SYSDATE, hire_date)) "Luni lucrate"
FROM employees
ORDER BY "Luni lucrate";


Exercițiul 13

SELECT last_name, hire_date, TO_CHAR(hire_date, 'DAY') "Zi"
FROM employees
ORDER BY TO_CHAR(hire_date, 'D');


Exercițiul 14

SELECT first_name || ' ' || last_name "Nume",
    NVL(TO_CHAR(commission_pct), 'Fara comision') "Comision"
FROM employees;


Exercițiul 15

SELECT last_name, salary, commission_pct
FROM employees
WHERE salary * (1 + NVL(commission_pct, 0)) >= 10000;


Exercițiul 16

SELECT first_name || ' ' || last_name "Nume",  job_id, salary,
    salary * (1 + DECODE(job_id,
                    'IT_PROG', 0.2,
                    'SA_REP', 0.25,
                    'SA_MAN', 0.35,
                    0)) "Salariu renegociat"
FROM employees;


Exercițiul 17

SELECT first_name, t2.department_id, t2.department_name
FROM employees t1
INNER JOIN departments t2
ON t1.department_id = t2.department_id;


Exercițiul 18

SELECT DISTINCT(job_id)
FROM employees t1
INNER JOIN departments t2
ON t1.department_id = t2.department_id
WHERE t1.department_id = 30;


Exercițiul 19

SELECT first_name, last_name, department_name, location_id
FROM employees t1
INNER JOIN departments t2
ON t1.department_id = t2.department_id
WHERE commission_pct IS NOT NULL;


Exercițiul 20

SELECT last_name, department_name
FROM employees t1
INNER JOIN departments t2
ON t1.department_id = t2.department_id
WHERE LOWER(last_name) LIKE '%a%';


Exercițiul 21

SELECT last_name, job_id, t2.department_id, t2.department_name
FROM employees t1
INNER JOIN departments t2
ON t1.department_id = t2.department_id
WHERE t2.location_id = (
    SELECT location_id
    FROM locations
    WHERE city LIKE '%Oxford%'
);


Exercițiul 22

SELECT
    t1.employee_id Ang#,
    t1.first_name || ' ' || t1.last_name Angajat,
    t2.employee_id Mgr#,
    t2.first_name || ' ' || t2.last_name Manager
FROM employees t1
INNER JOIN employees t2
ON t1.manager_id = t2.employee_id;


Exercițiul 23

SELECT
    t1.employee_id Ang#,
    t1.first_name || ' ' || t1.last_name Angajat,
    t2.employee_id Mgr#,
    t2.first_name || ' ' || t2.last_name Manager
FROM employees t1
LEFT OUTER JOIN employees t2
ON t1.manager_id = t2.employee_id;


Exercițiul 24

SELECT t1.last_name "Nume angajat", t1.department_id Departament, t2.last_name "Nume coleg"
FROM employees t1
JOIN employees t2
ON (t1.department_id = t2.department_id)
    AND (t1.employee_id <> t2.employee_id);


Exercițiul 25

SELECT t1.first_name || ' ' || t1.last_name name,
    t2.job_id,
    t2.job_title,
    t1.department_id,
    t1.salary
FROM employees t1
INNER JOIN jobs t2
ON t1.job_id = t2.job_id;


Exercițiul 26

SELECT first_name || ' ' || last_name name, hire_date
FROM employees
WHERE hire_date > (
    SELECT hire_date
    FROM employees
    WHERE last_name = 'Gates'
);


Exercițiul 27

SELECT
    t1.first_name || ' ' || t2.last_name Angajat,
    t1.hire_date Data_ang,
    t2.first_name || ' ' || t2.last_name Manager,
    t2.hire_date Data_mgr
FROM employees t1
INNER JOIN employees t2
ON t1.manager_id = t2.employee_id
WHERE t1.hire_date < t2.hire_date;
