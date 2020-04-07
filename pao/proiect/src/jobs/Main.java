package jobs;

import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;

import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        JobDatabase inMemoryDb = new InMemoryJobDatabase();

        DatabaseTest test = new DatabaseTest(inMemoryDb);
        test.runAllTests();

        try {
            CsvTest.writeToFile(inMemoryDb);
            JobDatabase newDb = CsvTest.readFromFile();
            System.out.println(newDb);
        } catch (IOException e) {
            System.err.println("CSV I/O tests failed");
            e.printStackTrace();
        }
    }
}
