package jobs.db.impl;

import jobs.db.JobDatabase;
import jobs.model.*;

import java.sql.*;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;

public final class SqliteDatabase implements JobDatabase {
    private final static String DATABASE_FILE = "jobs.db";
    private final Connection conn = DriverManager.getConnection("jdbc:sqlite:" + DATABASE_FILE);

    private final PreparedStatement insertCompanyStatement;
    private final PreparedStatement deleteCompanyStatement;

    private final PreparedStatement insertJobStatement;

    private final PreparedStatement insertUserStatement;
    private final PreparedStatement insertCandidateStatement;
    private final PreparedStatement insertRecruiterStatement;

    private final PreparedStatement insertCVStatement;

    private final PreparedStatement insertApplicationStatement;

    public SqliteDatabase() throws SQLException {
        ensureTablesCreated();

        insertCompanyStatement = conn.prepareStatement("INSERT INTO companies (id, name) VALUES (?, ?)");
        deleteCompanyStatement = conn.prepareStatement("DELETE FROM companies WHERE id = ?");

        insertJobStatement = conn.prepareStatement("INSERT INTO jobs" +
                "(id, title, timePosted, category, companyId) " +
                "VALUES (?, ?, ?, ?, ?)");

        insertUserStatement = conn.prepareStatement("INSERT INTO users" +
                "(id, firstName, initial, lastName)" +
                "VALUES (?, ?, ?, ?)");
        insertCandidateStatement = conn.prepareStatement("INSERT INTO candidates (userId) VALUES (?)");
        insertRecruiterStatement = conn.prepareStatement("INSERT INTO recruiters (userId, companyId) VALUES (?, ?)");

        insertCVStatement = conn.prepareStatement("INSERT INTO cvs (id, userId, description) VALUES (?, ?, ?)");

        insertApplicationStatement = conn.prepareStatement("INSERT INTO applications (jobId, cvId) VALUES (?, ?)");
    }

    @Override
    public void addCompany(Company company) {
        try {
            insertCompanyStatement.setInt(1, company.id);
            insertCompanyStatement.setString(2, company.name);
            checkRowInserted(insertCompanyStatement.executeUpdate());
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void removeCompany(Company company) {
        try {
            deleteCompanyStatement.setInt(1, company.id);
            checkRowDeleted(deleteCompanyStatement.executeUpdate());
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Collection<Company> getCompanies() {
        String getCompaniesSql = "SELECT * FROM companies";
        try (Statement stmt = conn.createStatement()) {
            ArrayList<Company> companies = new ArrayList<>();
            ResultSet result = stmt.executeQuery(getCompaniesSql);
            while (result.next()) {
                int id = result.getInt("id");
                String name = result.getString("name");
                Company company = new Company(id, name);
                companies.add(company);
            }
            return companies;
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void addJob(Job job) {
        try {
            insertJobStatement.setInt(1, job.id);
            insertJobStatement.setString(2, job.title);
            insertJobStatement.setString(3, job.timePosted.toString());
            insertJobStatement.setString(4, job.category.toString());
            insertJobStatement.setInt(5, job.companyId);
            checkRowInserted(insertJobStatement.executeUpdate());
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Collection<Job> getJobs() {
        String getJobsSql = "SELECT * FROM job";
        try (Statement stmt = conn.createStatement()) {
            ArrayList<Job> jobs = new ArrayList<>();
            ResultSet result = stmt.executeQuery(getJobsSql);
            while (result.next()) {
                int id = result.getInt("id");
                String title = result.getString("title");
                Instant timePosted = Instant.parse(result.getString("timePosted"));
                Category category = Category.valueOf(result.getString("category"));
                int companyId = result.getInt("companyId");
                Job job = new Job(id, title, timePosted, category, companyId);
                jobs.add(job);
            }
            return jobs;
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void addUser(User user) {
        try {
            insertUserStatement.setInt(1, user.id);
            insertUserStatement.setString(2, user.name.first);
            insertUserStatement.setString(3, user.name.initialOfFather);
            insertUserStatement.setString(4, user.name.last);
            checkRowInserted(insertUserStatement.executeUpdate());

            if (user instanceof Candidate) {
                insertCandidateStatement.setInt(1, user.id);
                checkRowInserted(insertCandidateStatement.executeUpdate());
            } else if (user instanceof Recruiter) {
                Recruiter recruiter = (Recruiter) user;
                insertRecruiterStatement.setInt(1, recruiter.id);
                insertRecruiterStatement.setInt(2, recruiter.companyId);
                checkRowInserted(insertRecruiterStatement.executeUpdate());
            } else {
                throw new UnsupportedOperationException();
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Collection<User> getUsers() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addCV(CV cv) {
        try {
            insertCVStatement.setInt(1, cv.id);
            insertCVStatement.setInt(2, cv.candidateId);
            insertCVStatement.setString(3, cv.description);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Collection<CV> getCVs(Candidate user) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addApplication(Application application) {
        try {
            insertApplicationStatement.setInt(1, application.jobId);
            insertApplicationStatement.setInt(2, application.cvId);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void removeApplication(Application application) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<Application> getApplications(Job job) {
        throw new UnsupportedOperationException();
    }

    private static void checkRowInserted(int insertedRows) {
        if (insertedRows != 1) {
            throw new RuntimeException("Failed to insert into database");
        }
    }

    private static void checkRowDeleted(int deletedRows) {
        if (deletedRows != 1) {
            throw new RuntimeException("Failed to delete row from database");
        }
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
