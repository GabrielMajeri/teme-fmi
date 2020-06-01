package jobs.db;

import jobs.model.*;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Date;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Wrapper providing audit and logging capabilities for an existing JobDatabase.
 */
public class DatabaseAudit implements JobDatabase {
    private final JobDatabase db;
    private final BufferedWriter logWriter;

    public DatabaseAudit(JobDatabase db, String logFilePath) throws IOException {
        this.db = db;

        Path path = Paths.get(logFilePath);
        this.logWriter = Files.newBufferedWriter(path, UTF_8);
    }

    @Override
    public void addCompany(Company company) {
        logAction("addCompany");
        db.addCompany(company);
    }

    @Override
    public void removeCompany(Company company) {
        logAction("removeCompany");
        db.removeCompany(company);
    }

    @Override
    public Collection<Company> getCompanies() {
        logAction("getCompanies");
        return db.getCompanies();
    }

    @Override
    public Company findCompanyByName(String name) {
        logAction("findCompanyByName");
        return db.findCompanyByName(name);
    }

    @Override
    public void addJob(Job job) {
        logAction("addJob");
        db.addJob(job);
    }

    @Override
    public Collection<Job> getJobs() {
        logAction("getJobs");
        return db.getJobs();
    }

    @Override
    public void addUser(User user) {
        logAction("addUser");
        db.addUser(user);
    }

    @Override
    public Collection<User> getUsers() {
        logAction("getUsers");
        return db.getUsers();
    }

    @Override
    public void addCV(CV cv) {
        logAction("addCV");
        db.addCV(cv);
    }

    @Override
    public Collection<CV> getCVs(Candidate user) {
        logAction("getCVs");
        return db.getCVs(user);
    }

    @Override
    public void addApplication(Application application) {
        logAction("addApplication");
        db.addApplication(application);
    }

    @Override
    public void removeApplication(Application application) {
        logAction("removeApplication");
        db.removeApplication(application);
    }

    @Override
    public Collection<Application> getApplications(Job job) {
        logAction("getApplications");
        return db.getApplications(job);
    }

    private void logAction(String actionName) {
        Date timestamp = new Date();
        String currentThreadName = Thread.currentThread().getName();
        String line = timestamp + " [" + actionName + "] @ " + currentThreadName + "\n";
        try {
            logWriter.write(line);
            logWriter.flush();
        } catch (IOException e) {
            System.err.print("Unable to write to log file");
            e.printStackTrace();
        }
    }

    @Override
    public String toString() {
        return "DatabaseAudit{" + db.toString() + "}";
    }
}
