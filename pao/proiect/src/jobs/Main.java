package jobs;

import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;
import jobs.db.impl.SqliteDatabase;

import java.io.IOException;
import java.sql.SQLException;

public class Main {
    public static void main(String[] args) {
        JobDatabase inMemoryDb = new InMemoryJobDatabase();

        DatabaseTest test = new DatabaseTest(inMemoryDb);
        test.runAllTests();

        try {
            JobDatabase sqliteDb = new SqliteDatabase();
        } catch (SQLException e) {
            e.printStackTrace();
        }

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
