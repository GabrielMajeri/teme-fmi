package jobs.model;

import java.util.Objects;

public class Application {
    public final Job job;
    public final CV cv;

    public Application(Job job, CV cv) {
        Objects.requireNonNull(job);
        Objects.requireNonNull(cv);

        this.job = job;
        this.cv = cv;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Application that = (Application) o;
        return job.equals(that.job) &&
                cv.equals(that.cv);
    }

    @Override
    public int hashCode() {
        return Objects.hash(job, cv);
    }
}
