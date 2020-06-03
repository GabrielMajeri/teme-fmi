package jobs.model;

import java.util.Objects;

public class Application {
    public final int jobId;
    public final int cvId;

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
