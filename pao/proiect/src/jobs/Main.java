package jobs;

import jobs.db.JobDatabase;
import jobs.db.impl.CsvDatabase;
import jobs.db.impl.InMemoryDatabase;
import jobs.db.impl.SqliteDatabase;
import jobs.utils.MockUtil;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

public class Main {
    public static void main(String[] args) {
        JobDatabase inMemoryDb = new InMemoryDatabase();

        MockUtil.fillDatabaseWithMockData(inMemoryDb, 7);

        CsvDatabase csvDatabase = CsvDatabase.INSTANCE;
        csvDatabase.reset();
        MockUtil.fillDatabaseWithMockData(csvDatabase, 3);

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
    }
}
