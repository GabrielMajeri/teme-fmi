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
