-- 2
DESCRIBE employees;
DESCRIBE departments;
DESCRIBE jobs;
DESCRIBE locations;

-- 3
SELECT first_name, last_name, department_id
FROM employees
WHERE department_id IN (10, 30)
ORDER BY first_name;

-- 4
SELECT sysdate
FROM dual;

SELECT TO_CHAR(sysdate, 'YYYY-MM-DD') AS iso_date
FROM dual;

-- 5
SELECT first_name, last_name, hire_date
FROM employees
WHERE hire_date LIKE '%87%';

SELECT first_name, last_name, hire_date
FROM employees
WHERE TO_CHAR(hire_date, 'YYYY') = '1987';

-- 6
SELECT first_name, last_name, job_id
FROM employees
WHERE manager_id IS NULL;

-- 7
SELECT first_name, last_name, salary, commission_pct
FROM employees
WHERE commission_pct > 0
ORDER BY salary DESC, commission_pct DESC;

-- 8
SELECT first_name, last_name, salary, commission_pct
FROM employees
ORDER BY salary DESC, commission_pct DESC;

-- 9
SELECT first_name, last_name
FROM employees
WHERE UPPER(first_name) LIKE '__A%';

-- 10
SELECT first_name, last_name
FROM employees
WHERE (UPPER(first_name) LIKE '%L%L%') AND
  ((department_id = 30) OR (manager_id = 102));

-- 11
SELECT first_name, last_name, job_id, salary
FROM employees
WHERE ((job_id LIKE '%CLERK%') OR (job_id LIKE '%REP%'))
    AND (salary NOT IN (1000, 2000, 3000));

-- 12
SELECT last_name, department_name
FROM employees e
INNER JOIN departments d
ON (e.department_id = d.department_id);

-- 13
SELECT department_name, first_name, last_name
FROM employees
RIGHT OUTER JOIN departments
USING (department_id);

-- 14
SELECT e1.employee_id, e1.first_name, e1.last_name,
    e2.employee_id, e2.first_name, e2.last_name
FROM employees e1
INNER JOIN employees e2
ON e1.manager_id = e2.employee_id;

-- 15
SELECT e1.employee_id, e1.first_name, e1.last_name,
    e2.employee_id, e2.first_name, e2.last_name
FROM employees e1
LEFT JOIN employees e2
ON e1.manager_id = e2.employee_id;

-- 16
SELECT d.department_id, COUNT(e.department_id)
FROM employees e
RIGHT JOIN departments d
ON e.department_id = d.department_id
GROUP BY d.department_id
HAVING COUNT(e.department_id) = 0;

-- 17
SELECT
    MAX(salary) AS Maxim,
    MIN(salary) AS Minim,
    SUM(salary) AS Suma,
    ROUND(AVG(salary), 2) AS Media
FROM employees;

-- 18
SELECT
    job_id,
    MIN(salary) AS Minim,
    MAX(salary) AS Maxim,
    SUM(salary) AS Suma,
    ROUND(AVG(salary), 2) AS Media
FROM employees
GROUP BY job_id;

-- 19
SELECT job_id, COUNT(1) AS employee_count
FROM employees
GROUP BY job_id;

-- 25
SELECT department_id, COUNT(1) AS num_employees
FROM employees
GROUP BY department_id
HAVING COUNT(1) >= 15;
