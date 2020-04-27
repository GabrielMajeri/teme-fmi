package jobs.db;

import jobs.model.Company;
import jobs.model.JobPosting;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;
import java.util.List;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Wrapper providing audit & logging capabilities for an existing JobDatabase.
 */
public class DatabaseAudit implements JobDatabase {
    private JobDatabase db;
    private BufferedWriter logWriter;

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
    public List<Company> getCompanies() {
        logAction("getCompanies");
        return db.getCompanies();
    }

    @Override
    public Company findCompanyByName(String name) {
        logAction("findCompanyByName");
        return db.findCompanyByName(name);
    }

    @Override
    public void addJob(JobPosting jobPosting) {
        logAction("addJob");
        db.addJob(jobPosting);
    }

    @Override
    public List<JobPosting> getJobPostings() {
        logAction("getJobPostings");
        return db.getJobPostings();
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
}
