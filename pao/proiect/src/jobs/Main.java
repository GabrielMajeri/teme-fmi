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
            Files.walk(Paths.get("."), 1)
                    .filter(fs.getPathMatcher("glob:**.{csv,db}")::matches)
                    .forEach(deleteFile);
        } catch (IOException e) {
            System.err.println("Unable to clean up data files");
            e.printStackTrace();
            System.exit(1);
        }

        for (JobDatabase db : loader) {
            MockUtil.fillDatabaseWithMockData(db, 7);

            new DatabaseTest(db).runAllTests();
        }
    }
}
