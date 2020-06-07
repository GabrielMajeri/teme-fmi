# Project: Job Platform

This is my Java project for the Advanced Object-oriented Programming course.
It is a data management system for a job searching platform.

## Modules

The following is a high-level overfiew of the architecture:

- `csv`: generic interfaces and classes for reading from/writing to CSV files.

- `jobs`: classes and interfaces specific to the problem.
  - `db`: database services.
  - `model`: data model implemented as Plain Old Java Objects.
  - `gui`: Swing graphical user interface.
  - `tests`: unit and integration tests.
  - `utils`: utility classes.

## Entry points (main classes)

- `jobs.tests.TestMain`: runs tests on the database implementations.
- `jobs.gui.MainWindow`: opens a Swing GUI for interacting with the database.
