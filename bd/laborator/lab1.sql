Exercițiul 3

DESC employees;


Exercițiul 4

SELECT * FROM employees;


Exercițiul 5

SELECT employee_id, first_name, last_name, job_id, hire_date
FROM employees;


Exercițiul 6

SELECT job_id FROM employees;
SELECT DISTINCT(job_id) FROM employees;


Exercițiul 7

SELECT first_name || ' ' || last_name "Nume angajat"
FROM employees;


Exercițiul 8

SELECT first_name || ',' || last_name || ',' || email || ',' || phone_number || ',' || job_id || ',' || salary "Informatii complete"
FROM employees;


Exercițiul 9

SELECT first_name || ' ' || last_name "Nume", salary "Salariu"
FROM employees
WHERE salary > 2850;


Exercițiul 10

SELECT first_name || ' ' || last_name "Nume", department_id
FROM employees
WHERE employee_id = 104;


Exercițiul 11

SELECT first_name || ' ' || last_name "Nume", salary
FROM employees
WHERE salary NOT BETWEEN 1500 AND 2850;


Exercițiul 12

SELECT first_name || ' ' || last_name "Nume", job_id, hire_date
FROM employees
WHERE hire_date BETWEEN '20-FEB-1987' AND '1-MAY-1989'
ORDER BY hire_date;


Exercițiul 13

SELECT first_name || ' ' || last_name employee_name, department_id
FROM employees
WHERE department_id IN (10, 30)
ORDER BY employee_name;


Exercițiul 14

SELECT first_name || ' ' || last_name "Nume", salary "Salariu lunar"
FROM employees
WHERE (salary > 3500) AND (department_id IN (10, 30));


Exercițiul 15

Cum selectezi data de azi:
SELECT sysdate FROM dual;


Exercițiul 16

SELECT first_name || ' ' || last_name "Nume", hire_date
FROM employees
WHERE hire_date LIKE '%87%';

SELECT first_name || ' ' || last_name "Nume", hire_date
FROM employees
WHERE TO_CHAR(hire_date, 'YYYY') = '1987';


Exercițiul 17

SELECT first_name || ' ' || last_name "Nume", job_id
FROM employees
WHERE manager_id IS null;


Exercițiul 18

SELECT first_name || ' ' || last_name "Nume", salary, commission_pct
FROM employees
WHERE commission_pct IS NOT null
ORDER BY salary DESC, commission_pct DESC;


Exercițiul 19

SELECT first_name || ' ' || last_name "Nume", salary, commission_pct
FROM employees
ORDER BY salary DESC, commission_pct DESC;

null vine înainte de celelalte valori.


Exercițiul 20

SELECT first_name || ' ' || last_name "Nume"
FROM employees
WHERE first_name LIKE '__a%';

SELECT first_name || ' ' || last_name "Nume", department_id, manager_id
FROM employees
WHERE (first_name LIKE '%l%l%') AND ((department_id = 30) OR (manager_id = 101));


Exercițiul 22

SELECT first_name || ' ' || last_name "Nume", job_id, salary
FROM employees
WHERE ((job_id LIKE '%CLERK%') OR (job_id LIKE '%REP%'))
  AND (salary NOT IN (1000, 2000, 3000));


Exercițiul 23

SELECT first_name || ' ' || last_name nume, job_id, salary
FROM employees
WHERE salary > 5 * (salary * commission_pct);
