package jobs.model;

import csv.CsvTypeFactory;

import java.util.Objects;

public class Application {
    public final int jobId;
    public final int cvId;

    public final static CsvTypeFactory<Application> FACTORY = new CsvTypeFactory<Application>() {
        @Override
        public String[] getColumnNames() {
            return new String[]{"jobId", "cvId"};
        }

        @Override
        public String[] toStringArray(Application application) {
            String jobId = Integer.toString(application.jobId);
            String cvId = Integer.toString(application.cvId);
            return new String[]{jobId, cvId};
        }

        @Override
        public Application fromStringArray(String[] data) {
            int jobId = Integer.parseInt(data[0]);
            int cvId = Integer.parseInt(data[1]);
            return new Application(jobId, cvId);
        }
    };

    public Application(int jobId, int cvId) {
        this.jobId = jobId;
        this.cvId = cvId;
    }

    public Application(Job job, CV cv) {
        this(Objects.requireNonNull(job).id, Objects.requireNonNull(cv).id);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Application that = (Application) o;
        return jobId == that.jobId && cvId == that.cvId;
    }

    @Override
    public int hashCode() {
        return Objects.hash(jobId, cvId);
    }
}
