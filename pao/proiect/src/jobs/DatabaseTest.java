package jobs;

import jobs.db.JobDatabase;

public class DatabaseTest {
    private final JobDatabase db;

    public DatabaseTest(JobDatabase db) {
        this.db = db;
    }

    public void runAllTests() {
        db.getCompanies().stream()
                .filter(company -> company.name.equals("Mega Image"))
                .findAny().get();
    }
}
