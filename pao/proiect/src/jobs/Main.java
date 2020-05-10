package jobs;

import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;
import jobs.db.impl.SqliteDatabase;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

public class Main {
    public static void main(String[] args) {
        JobDatabase inMemoryDb = new InMemoryJobDatabase();

        MockUtil.fillDatabaseWithMockData(inMemoryDb, 7);

        System.out.println(inMemoryDb);

        try {
            File dbFile = new File("jobs.db");
            if (dbFile.exists()) {
                dbFile.delete();
            }

            JobDatabase sqliteDb = new SqliteDatabase();
            new DatabaseTest(sqliteDb).runAllTests();
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
