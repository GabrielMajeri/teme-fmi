package jobs.db.impl;

import jobs.db.JobDatabase;
import jobs.model.Company;
import jobs.model.JobPosting;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public final class SqliteDatabase implements JobDatabase {
    private Connection conn = null;

    public SqliteDatabase() throws SQLException {
        conn = DriverManager.getConnection("jdbc:sqlite:jobs.db");

        String createCompaniesTableSql =
                "CREATE TABLE IF NOT EXISTS companies (" +
                    "id INTEGER PRIMARY KEY, " +
                    "name VARCHAR(32) NOT NULL" +
                ")";

        try (Statement stmt = conn.createStatement()) {
            stmt.execute(createCompaniesTableSql);
        }
    }

    @Override
    public void addCompany(Company company) {
        String insertCompanySql =
                "INSERT INTO companies " +
                "(id, name) " +
                "VALUES " +
                "('" + company.getId() + "', '" + company.getName() + "')";
        try (Statement stmt = conn.createStatement()) {
            int insertedRows = stmt.executeUpdate(insertCompanySql);
            if (insertedRows != 1) {
                throw new SQLException("Failed to insert company into database");
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Company> getCompanies() {
        String getCompaniesSql = "SELECT name, id FROM companies";
        try (Statement stmt = conn.createStatement()) {
            ArrayList<Company> companies = new ArrayList<>();
            ResultSet result = stmt.executeQuery(getCompaniesSql);
            while (result.next()) {
                Company company = new Company(result.getString("name"), result.getInt("id"));
                companies.add(company);
            }
            return companies;
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Company findCompanyByName(String name) {
        String getCompanyByNameSql =
                "SELECT name, id FROM companies " +
                "WHERE name = '" + name + "'";
        try (Statement stmt = conn.createStatement()) {
            ResultSet result = stmt.executeQuery(getCompanyByNameSql);
            if (result.next()) {
                return new Company(result.getString("name"), result.getInt("id"));
            }
            return null;
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void addJob(JobPosting jobPosting) {
    }

    @Override
    public List<JobPosting> getJobPostings() {
        return null;
    }
}
