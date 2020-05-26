# Project: Job Platform

This is my Java project for the Advanced Object-oriented Programming course.
It is a data management system for a job searching platform.

## Modules

- `jobs.db`: `JobDatabase` service as an abstract interface.
  The various database implementations (in-memory, CSV files, SQL)
  can be found in the `jobs.db.impl` package.

- `jobs.model`: data model implemented as Plain Old Java Objects.

- `jobs.gui`: Swing graphical user interface for interacting with the database.
