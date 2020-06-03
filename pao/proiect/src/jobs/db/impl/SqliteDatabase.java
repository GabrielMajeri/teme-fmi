package jobs.db.impl;

import jobs.db.JobDatabase;
import jobs.model.*;

import java.sql.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public final class SqliteDatabase implements JobDatabase {
    private final static String DATABASE_FILE = "jobs.db";
    private final Connection conn = DriverManager.getConnection("jdbc:sqlite:" + DATABASE_FILE);

    public SqliteDatabase() throws SQLException {
        ensureTablesCreated();
    }

    @Override
    public void addCompany(Company company) {
        String insertCompanySql =
                "INSERT INTO companies " +
                "(id, name) " +
                "VALUES " +
                "('" + company.id + "', '" + company.name + "')";
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
    public void removeCompany(Company company) {
        throw new UnsupportedOperationException();
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
    public void addJob(Job job) {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<Job> getJobs() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addUser(User user) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<User> getUsers() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addCV(CV cv) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<CV> getCVs(Candidate user) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addApplication(Application application) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void removeApplication(Application application) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<Application> getApplications(Job job) {
        throw new UnsupportedOperationException();
    }

    private void ensureTablesCreated() throws SQLException {
        String createCompaniesTableSql =
                "CREATE TABLE IF NOT EXISTS companies (" +
                        "id INTEGER PRIMARY KEY, " +
                        "name TEXT NOT NULL" +
                        ")";

        String createUsersTableSql =
                "CREATE TABLE IF NOT EXISTS users (" +
                        "id INTEGER PRIMARY KEY, " +
                        "firstName TEXT NOT NULL, " +
                        "initial TEXT NOT NULL, " +
                        "lastName TEXT NOT NULL" +
                        ")";

        String createRecruitersTableSql =
                "CREATE TABLE IF NOT EXISTS recruiters (" +
                        "userId INTEGER PRIMARY KEY, " +
                        "companyId INTEGER NOT NULL, " +
                        "FOREIGN KEY (userId) REFERENCES users(id), " +
                        "FOREIGN KEY (companyId) REFERENCES companies(id)" +
                        ")";

        String createCandidatesTableSql =
                "CREATE TABLE IF NOT EXISTS candidates (" +
                        "userId INTEGER PRIMARY KEY, " +
                        "FOREIGN KEY (userId) REFERENCES users(id)" +
                        ")";

        String createCvsTableSql =
                "CREATE TABLE IF NOT EXISTS cvs (" +
                        "id INTEGER PRIMARY KEY, " +
                        "userId INTEGER NOT NULL, " +
                        "description TEXT NOT NULL, " +
                        "FOREIGN KEY (userId) REFERENCES users(id)" +
                        ")";

        String createJobsTableSql =
                "CREATE TABLE IF NOT EXISTS jobs (" +
                        "id INTEGER PRIMARY KEY, " +
                        "title TEXT NOT NULL, " +
                        "timePosted TEXT NOT NULL, " +
                        "category TEXT NOT NULL, " +
                        "companyId INTEGER NOT NULL, " +
                        "FOREIGN KEY (companyId) REFERENCES companies(id)" +
                        ")";

        String createApplicationsTableSql =
                "CREATE TABLE IF NOT EXISTS applications (" +
                        "jobId INTEGER NOT NULL, " +
                        "cvId INTEGER NOT NULL, " +
                        "FOREIGN KEY (jobId) REFERENCES jobs(id), " +
                        "FOREIGN KEY (cvId) REFERENCES cvs(id)" +
                        ")";

        try (Statement stmt = conn.createStatement()) {
            stmt.execute(createCompaniesTableSql);
            stmt.execute(createUsersTableSql);
            stmt.execute(createRecruitersTableSql);
            stmt.execute(createCandidatesTableSql);
            stmt.execute(createCvsTableSql);
            stmt.execute(createJobsTableSql);
            stmt.execute(createApplicationsTableSql);
        }
    }
}
