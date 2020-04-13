package jobs.db.impl;

import jobs.db.JobDatabase;
import jobs.model.Company;
import jobs.model.Job;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

public final class SqliteDatabase implements JobDatabase {
    private Connection conn = null;

    public SqliteDatabase() throws SQLException {
        conn = DriverManager.getConnection("jdbc:sqlite:jobs.db");
    }

    @Override
    public void addCompany(Company company) {

    }

    @Override
    public List<Company> getCompanies() {
        return null;
    }

    @Override
    public Company findCompanyByName(String name) {
        return null;
    }

    @Override
    public void addJob(Job job) {

    }

    @Override
    public List<Job> getJobPostings() {
        return null;
    }
}
