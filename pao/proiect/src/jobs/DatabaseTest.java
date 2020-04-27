package jobs;

import jobs.db.JobDatabase;
import jobs.model.Category;
import jobs.model.Company;
import jobs.model.JobPosting;

import java.util.Date;

public class DatabaseTest {
    private JobDatabase db;

    public DatabaseTest(JobDatabase db) {
        this.db = db;
        fillDatabaseWithMockData(db);
    }

    public void runAllTests() {
        Company megaImage = db.findCompanyByName("Mega Image");
        if (megaImage == null) {
            throw new RuntimeException("Test failed: cannot find previously inserted company");
        }
    }

    public static void fillDatabaseWithMockData(JobDatabase db) {
        Company endava = new Company("Endava", 9533457);
        db.addCompany(endava);
        Company megaImage = new Company("Mega Image", 6719278);
        db.addCompany(megaImage);

        Job storeClerk = new Job("Store clerk", new Date(), Category.Sales, megaImage);
        db.addJob(storeClerk);

        Job juniorJavaDeveloper = new Job("Junior Java Developer", new Date(), Category.IT, endava);
        db.addJob(juniorJavaDeveloper);

        Job seniorFullStackEngineer = new Job("Senior Full-stack Engineer", new Date(), Category.IT, endava);
        db.addJob(seniorFullStackEngineer);
    }
}
