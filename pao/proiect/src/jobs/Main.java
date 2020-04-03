package jobs;

import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;

public class Main {
    public static void main(String[] args) {
        JobDatabase inMemoryDb = new InMemoryJobDatabase();

        DatabaseTest test = new DatabaseTest(inMemoryDb);
        test.runAllTests();
    }
}
