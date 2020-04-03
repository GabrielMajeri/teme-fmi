Exercițiul 1

a) Nu, funcțiile grup ignoră valorile NULL.
b) WHERE se aplică pe date înainte de agregare, în timp ce
   HAVING filtrează grupurile formate.


Exercițiul 2

SELECT
    ROUND(MAX(salary)) AS Maxim,
    ROUND(MIN(salary)) AS Minim,
    ROUND(SUM(salary)) AS Suma,
    ROUND(AVG(salary)) AS Media
FROM employees;


Exercițiul 3

SELECT
    job_id AS Job,
    ROUND(MAX(salary)) AS Maxim,
    ROUND(MIN(salary)) AS Minim,
    ROUND(SUM(salary)) AS Suma,
    ROUND(AVG(salary)) AS Media
FROM employees
GROUP BY job_id;


Exercițiul 4

SELECT job_id AS Job, COUNT(1) AS NrAngajati
FROM employees
GROUP BY job_id;


Exercițiul 5

SELECT COUNT(DISTINCT manager_id) AS "Nr. manageri"
FROM employees;


Exercițiul 6

SELECT MAX(salary) - MIN(salary) AS DifSalariu
FROM employees;


Exercițiul 7

SELECT
    department_name AS "Nume departament",
    location_id AS "Locatie",
    COUNT(employee_id) AS "Nr. angajati",
    ROUND(AVG(salary)) AS "Salariu mediu"
FROM departments
JOIN employees
USING (department_id)
JOIN locations
USING (location_id)
GROUP BY department_name, location_id;


Exercițiul 8

SELECT employee_id, last_name
FROM employees
WHERE salary >= (SELECT AVG(salary) FROM employees)
ORDER BY salary DESC;


Exercițiul 9

SELECT manager_id, MIN(salary)
FROM employees
WHERE manager_id IS NOT NULL
GROUP BY manager_id
HAVING MIN(salary) >= 1000
ORDER BY MIN(salary) DESC;


Exercițiul 10

SELECT department_id, department_name, MAX(salary)
FROM departments
JOIN employees
USING (department_id)
GROUP BY department_id, department_name
HAVING MAX(salary) >= 3000;


Exercițiul 11

SELECT MIN(AVG(salary))
FROM employees
GROUP BY job_id;


Exercițiul 12

SELECT department_id, department_name, SUM(salary)
FROM departments
JOIN employees
USING (department_id)
GROUP BY department_id, department_name;


Exercițiul 13

SELECT MAX(avg_salary) AS max_avg_salary
FROM (
    SELECT department_id, ROUND(AVG(salary)) AS avg_salary
    FROM employees
    GROUP BY department_id
);


Exercițiul 14

SELECT job_id, job_title, AVG(salary)
FROM jobs
JOIN employees
USING (job_id)
GROUP BY job_id, job_title
HAVING AVG(salary) = (
    SELECT MIN(AVG(salary))
    FROM employees
    GROUP BY job_id
);


Exercițiul 15

SELECT AVG(salary)
FROM employees
HAVING AVG(salary) >= 2500;


Exercițiul 16

SELECT department_id, job_id, SUM(salary)
FROM departments
JOIN employees
USING (department_id)
GROUP BY department_id, job_id;


Exercițiul 17

SELECT department_name, MIN(salary)
FROM departments
JOIN employees
USING (department_id)
GROUP BY department_name
HAVING AVG(salary) = (
    SELECT MIN(AVG(salary))
    FROM employees
    GROUP BY department_id
);


Exercițiul 18

a)

SELECT department_id, department_name, COUNT(1)
FROM departments
INNER JOIN employees
USING (department_id)
GROUP BY department_id, department_name
HAVING COUNT(1) < 4;

b)

SELECT department_id, department_name, COUNT(1)
FROM departments
INNER JOIN employees
USING (department_id)
GROUP BY department_id, department_name
HAVING COUNT(1) = (
    SELECT MAX(COUNT(1))
    FROM employees
    GROUP BY department_id
);


Exercițiul 19

SELECT last_name
FROM employees
WHERE hire_date = (
    SELECT hire_date
    FROM employees
    GROUP BY hire_date
    HAVING COUNT(1) = (
        SELECT MAX(COUNT(1))
        FROM employees
        GROUP BY hire_date
    )
);


Exercițiul 20

SELECT COUNT(1)
FROM departments
WHERE department_id IN (
    SELECT department_id
    FROM departments
    INNER JOIN employees
    USING (department_id)
    GROUP BY department_id
    HAVING COUNT(1) >= 15
);


Exercițiul 21

SELECT department_id, SUM(salary)
FROM departments
INNER JOIN employees
USING (department_id)
GROUP BY department_id
HAVING COUNT(1) >= 1 AND department_id != 30
ORDER BY SUM(salary);


Exercițiul 22

SELECT *
FROM (
    SELECT department_id, department_name,
        COUNT(1) AS "Numar angajati",
        AVG(salary) AS "Salariu mediu"
    FROM departments
    LEFT JOIN employees
    USING (department_id)
    GROUP BY department_id, department_name
)
LEFT JOIN (
    SELECT last_name, salary, job_id, department_id
    FROM employees
)
USING (department_id);


Exercițiul 23

SELECT department_id, city, job_id, SUM(salary)
FROM departments
INNER JOIN employees USING (department_id)
INNER JOIN locations USING (location_id)
WHERE department_id > 80
GROUP BY department_id, city, job_id;

Diferența este că soluția cu WHERE este mai eficientă, deoarece valorile
care nu ne interesează nu mai ajung nici măcar la GROUP BY.


Exercițiul 24

SELECT last_name
FROM employees
WHERE employee_id IN (
    SELECT employee_id
    FROM employees
    INNER JOIN job_history USING (employee_id)
    GROUP BY employee_id
    HAVING COUNT(1) >= 2
);


Exercițiul 25

SELECT AVG(NVL(commission_pct, 0))
FROM employees;


Exercițiul 26

SELECT department_id, TO_CHAR(hire_date, 'yyyy'), SUM(salary)
FROM employees
WHERE department_id < 50
GROUP BY ROLLUP(department_id, TO_CHAR(hire_date, 'yyyy'));

SELECT department_id, TO_CHAR(hire_date, 'yyyy'), SUM(salary)
FROM employees
WHERE department_id < 50
GROUP BY CUBE(department_id, TO_CHAR(hire_date, 'yyyy'));


Exercițiul 27

SELECT
    job_id AS Job,
    (
        SELECT SUM(e.salary)
        FROM employees e
        WHERE e.department_id = 30 AND e.job_id = j.job_id
    ) AS Dep30,
    (
        SELECT SUM(e.salary)
        FROM employees e
        WHERE e.department_id = 50 AND e.job_id = j.job_id
    ) AS Dep50,
    (
        SELECT SUM(e.salary)
        FROM employees e
        WHERE e.department_id = 80 AND e.job_id = j.job_id
    ) AS Dep80,
    (
        SELECT SUM(e.salary)
        FROM employees e
        WHERE e.job_id = j.job_id
    ) AS Total
FROM jobs j;


Exercițiul 28

SELECT
    (
        SELECT COUNT(1)
        FROM employees
    ) AS Total,
    (
        SELECT COUNT(1)
        FROM employees e
        WHERE TO_CHAR(e.hire_date, 'yyyy') = '1997'
    ) AS Hired1997,
    (
        SELECT COUNT(1)
        FROM employees e
        WHERE TO_CHAR(e.hire_date, 'yyyy') = '1998'
    ) AS Hired1998,
    (
        SELECT COUNT(1)
        FROM employees e
        WHERE TO_CHAR(e.hire_date, 'yyyy') = '1999'
    ) AS Hired1999,
    (
        SELECT COUNT(1)
        FROM employees e
        WHERE TO_CHAR(e.hire_date, 'yyyy') = '2000'
    ) AS Hired2000
FROM dual;


Exercițiul 29

SELECT department_id, department_name,
    (
        SELECT COUNT(1)
        FROM employees e
        WHERE e.department_id = department_id
    ) AS "Numar angajati",
    (
        SELECT AVG(e.salary)
        FROM employees e
        WHERE e.department_id = department_id
    ) AS "Salariu mediu",
    last_name,
    salary,
    job_id
FROM departments
LEFT JOIN employees
USING (department_id);


Exercițiul 30

FROM (
    SELECT department_id, SUM(salary) AS sum_salaries
    FROM employees
    GROUP BY department_id
)
INNER JOIN departments USING (department_id);


Exercițiul 31

SELECT last_name, salary, department_id, avg_salary
FROM (
    SELECT department_id, AVG(salary) AS avg_salary
    FROM employees
    GROUP BY department_id
)
INNER JOIN employees USING (department_id);


Exercițiul 32

SELECT last_name, salary, department_id, avg_salary, num_employees
FROM (
    SELECT
        department_id,
        AVG(salary) AS avg_salary,
        COUNT(1) AS num_employees
    FROM employees
    GROUP BY department_id
)
INNER JOIN employees USING (department_id);


Exercițiul 33

SELECT department_name, first_name, last_name, salary
FROM (
    SELECT department_name, department_id, MIN(salary) AS min_salary
    FROM departments
    INNER JOIN employees USING (department_id)
    GROUP BY department_name, department_id
)
INNER JOIN employees USING (department_id)
WHERE salary = min_salary;


Exercițiul 34

SELECT department_id, department_name, employee_count, average_salary,
    first_name, last_name, salary, job_id
FROM (
    SELECT
        department_id,
        COUNT(1) AS employee_count,
        AVG(salary) AS average_salary
    FROM employees
    GROUP BY department_id
)
INNER JOIN employees USING (department_id)
FULL OUTER JOIN departments USING (department_id);
