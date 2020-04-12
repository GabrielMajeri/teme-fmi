1.

a)
SELECT department_name, job_title, AVG(salary)
FROM employees
INNER JOIN departments USING (department_id)
INNER JOIN jobs USING (job_id)
GROUP BY ROLLUP (department_name, job_title);

b)
SELECT department_name, job_title, AVG(salary),
    NVL2(department_name,
        NVL2(job_title,
            'per departament',
            'per job'),
        'per total') AS "tip"
FROM employees
INNER JOIN departments USING (department_id)
INNER JOIN jobs USING (job_id)
GROUP BY ROLLUP (department_name, job_title);


2.

a)
SELECT department_name, job_title, AVG(salary)
FROM employees
INNER JOIN departments USING (department_id)
INNER JOIN jobs USING (job_id)
GROUP BY
    GROUPING SETS (
        (department_name, job_title),
        (department_name),
        (job_title),
        ()
    );

b)
SELECT department_name, job_title, AVG(salary),
    CASE GROUPING(department_name)
    WHEN 1 THEN 'Dep'
    ELSE ''
    END ||
    CASE GROUPING(job_title)
    WHEN 1 THEN 'Job'
    ELSE ''
    END AS coloana
FROM employees
INNER JOIN departments USING (department_id)
INNER JOIN jobs USING (job_id)
GROUP BY
    GROUPING SETS (
        (department_name, job_title),
        (department_name),
        (job_title),
        ()
    );


3.

SELECT
    department_name, job_title,
    departments.manager_id,
    MAX(salary),
    AVG(salary)
FROM employees
INNER JOIN departments USING (department_id)
INNER JOIN jobs USING (job_id)
GROUP BY
    GROUPING SETS (
        (department_name, job_title),
        (job_title, departments.manager_id),
        ()
    );


4.

SELECT MAX(salary)
FROM employees
HAVING MAX(salary) >= 15000;


5.
a)
SELECT e.last_name, e.salary
FROM employees e
WHERE salary >= (
    SELECT AVG(salary)
    FROM employees
    WHERE department_id = e.department_id
);

b)
SELECT
    e.last_name,
    e.salary,
    d.department_name,
    deps.avg_salary,
    deps.employee_count
FROM employees e
INNER JOIN (
    SELECT
        department_id,
        AVG(salary) AS avg_salary,
        COUNT(1) AS employee_count
    FROM employees
    GROUP BY department_id
) deps
ON e.department_id = deps.department_id
INNER JOIN departments d
ON e.department_id = d.department_id
WHERE salary >= (
    SELECT AVG(salary)
    FROM employees
    WHERE department_id = e.department_id
);

SELECT e.last_name, e.salary,
    d.department_name,
    (
        SELECT AVG(salary)
        FROM employees
        WHERE department_id = e.department_id
    ) AS avg_salary,
    (
        SELECT COUNT(1)
        FROM employees
        WHERE department_id = e.department_id
    ) AS num_employees
FROM employees e
INNER JOIN departments d
ON e.department_id = d.department_id
WHERE salary >= (
    SELECT AVG(salary)
    FROM employees
    WHERE department_id = e.department_id
);


6.

SELECT last_name, salary
FROM employees
WHERE salary >= ALL(
    SELECT AVG(salary)
    FROM employees
    GROUP BY department_id
);

SELECT last_name, salary
FROM employees
WHERE salary >= (
    SELECT MAX(AVG(salary))
    FROM employees
    GROUP BY department_id
);


7.

SELECT department_id, last_name, salary
FROM employees e
WHERE salary = (
    SELECT MIN(salary)
    FROM employees
    WHERE department_id = e.department_id
);


SELECT d.department_id, e.last_name, e.salary
FROM (
    SELECT department_id, MIN(salary) AS min_salary
    FROM employees
    GROUP BY department_id
) d
INNER JOIN employees e
ON e.department_id = d.department_id AND
    e.salary = d.min_salary;


8.

SELECT t3.department_name, t1.last_name
FROM employees t1
INNER JOIN departments t3
ON t1.department_id = t3.department_id
WHERE hire_date = (
    SELECT MIN(hire_date)
    FROM employees t2
    WHERE t2.department_id = t1.department_id
)
ORDER BY t3.department_name;


9.

SELECT department_id, last_name
FROM employees
WHERE department_id IN (
    SELECT DISTINCT department_id
    FROM employees e
    WHERE EXISTS (
        SELECT 1
        FROM employees
        WHERE department_id = e.department_id AND salary = (
            SELECT MAX(salary)
            FROM employees
            WHERE department_id = 30
        )
    )
);


10.

SELECT salary, last_name
FROM employees
WHERE salary IN (
    SELECT salary
    FROM (
        SELECT salary
        FROM employees
        ORDER BY salary DESC
    ) WHERE rownum <= 3
)
ORDER BY salary;


11.

SELECT e.employee_id, e.last_name, e.first_name
FROM employees e
WHERE 2 <= (
    SELECT COUNT(1)
    FROM employees
    WHERE manager_id = e.employee_id
);


12.

SELECT l.location_id, l.city
FROM locations l
WHERE EXISTS (
    SELECT 1
    FROM departments d
    WHERE d.location_id = l.location_id
);


13.

SELECT DISTINCT d.department_id
FROM departments d
WHERE NOT EXISTS (
    SELECT 1
    FROM employees e
    WHERE e.department_id = d.department_id
);


14.

a)
SELECT employee_id, last_name, hire_date, salary, manager_id
FROM employees
WHERE LEVEL = 2
START WITH last_name = 'De Haan'
CONNECT BY PRIOR employee_id = manager_id;

b)
SELECT employee_id, last_name, hire_date, salary, manager_id
FROM employees
START WITH last_name = 'De Haan'
CONNECT BY PRIOR employee_id = manager_id;


15.

SELECT employee_id, last_name, manager_id
FROM employees
START WITH employee_id = 114
CONNECT BY PRIOR employee_id = manager_id;


16.

SELECT employee_id, manager_id, last_name, LEVEL
FROM employees
WHERE LEVEL = 3
START WITH last_name = 'De Haan'
CONNECT BY PRIOR employee_id = manager_id;


17.

SELECT employee_id, manager_id, LEVEL, last_name
FROM employees
CONNECT BY PRIOR manager_id = employee_id;


18.

SELECT employee_id, last_name, salary, LEVEL, manager_id
FROM employees
WHERE salary > 5000
START WITH salary = (
    SELECT MAX(salary)
    FROM employees
)
CONNECT BY PRIOR employee_id = manager_id;


19.

WITH t AS (
    SELECT department_name, SUM(salary) AS total_salary
    FROM employees
    INNER JOIN departments
    USING (department_id)
    GROUP BY department_name
)
SELECT department_name, total_salary
FROM t
WHERE total_salary > (
    SELECT AVG(total_salary)
    FROM t
);


20.

WITH t1 AS (
    SELECT employee_id, hire_date
    FROM employees
    START WITH first_name = 'Steven' AND last_name = 'King'
    CONNECT BY PRIOR employee_id = manager_id
),
t2 AS (
    SELECT employee_id
    FROM t1
    WHERE hire_date = (
        SELECT MIN(hire_date)
        FROM t1
    )
)
SELECT employee_id, first_name || ' ' || last_name AS name, job_id, hire_date
FROM employees
WHERE TO_CHAR(hire_date, 'YYYY') != 1970
START WITH employee_id IN (SELECT * FROM t2)
CONNECT BY PRIOR employee_id = manager_id;


21.
SELECT *
FROM (
    SELECT last_name, salary
    FROM employees
    ORDER BY salary DESC
)
WHERE ROWNUM <= 10;


22.

SELECT *
FROM (
    SELECT job_id, AVG(salary) AS avg_salary
    FROM employees
    GROUP BY job_id
    ORDER BY avg_salary
)
WHERE ROWNUM <= 3;


23.

SELECT d.department_name || ' este condus de ' ||
    NVL(TO_CHAR(t1.manager_id), 'nimeni') || ' si ' ||
    NVL2(
        t2.employee_count,
        ' are numarul de salariati ' || TO_CHAR(t2.employee_count),
        ' nu are salariati'
    )
    AS info
FROM departments d
LEFT JOIN (
    SELECT department_id, manager_id
    FROM employees
    GROUP BY department_id, manager_id
) t1
ON d.department_id = t1.department_id
LEFT JOIN (
    SELECT department_id, COUNT(1) AS employee_count
    FROM employees
    GROUP BY department_id
) t2
ON d.department_id = t2.department_id;


24.

SELECT
    last_name, first_name,
    NVL2(
        NULLIF(LENGTH(last_name), LENGTH(first_name)),
        TO_CHAR(LENGTH(last_name)),
        'same len'
    ) AS last_name_len
FROM employees;


25.

SELECT last_name, hire_date, salary,
    salary * DECODE(TO_CHAR(hire_date, 'YYYY'),
        1989, 1.2,
        1990, 1.15,
        1991, 1.1,
        1
    ) AS new_salary
FROM employees;


26.

SELECT
    job_id,
    CASE
        WHEN job_id LIKE 'S%' THEN SUM(salary)
        WHEN MAX(salary) = (SELECT MAX(salary) FROM employees) THEN AVG(salary)
        ELSE MIN(salary)
    END AS info
FROM employees
GROUP BY job_id;
