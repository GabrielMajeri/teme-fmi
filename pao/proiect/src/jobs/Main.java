package jobs;

import jobs.db.JobDatabase;
import jobs.utils.MockUtil;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.util.ServiceLoader;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

public class Main {
    public static void main(String[] args) {
        ServiceLoader<JobDatabase> loader = ServiceLoader.load(JobDatabase.class);

        FileSystem fs = FileSystems.getDefault();

        try {
            Consumer<Path> deleteFile = path -> {
                try {
                    Files.delete(path);
                } catch (IOException e) {
                    System.err.println("Unable to delete file " + path);
                }
            };

            // Clean up .csv and .db files
            Predicate<Path> csvPredicate = fs.getPathMatcher("glob:*.csv")::matches;
            Predicate<Path> dbPredicate = fs.getPathMatcher("glob:*.db")::matches;
            Predicate<Path> fileExtPredicate = csvPredicate.or(dbPredicate);

            Files.walk(Paths.get("."), 0).filter(fileExtPredicate).forEach(deleteFile);
        } catch (IOException e) {
            System.err.println("Unable to clean up data files");
            e.printStackTrace();
            System.exit(1);
        }

        for (JobDatabase db : loader) {
            if (!db.getClass().getSimpleName().equals("InMemoryDatabase")) {
                continue;
            }

            MockUtil.fillDatabaseWithMockData(db, 7);

            new DatabaseTest(db).runAllTests();
        }
    }
}
