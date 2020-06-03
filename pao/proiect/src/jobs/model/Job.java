package jobs.model;

import jobs.utils.IdAllocator;

import java.time.Instant;
import java.util.Objects;

public class Job {
    private final static IdAllocator jobIds = new IdAllocator(1, 15000);

    public final int id;
    public final String title;
    public final Instant timePosted;
    public final Category category;
    public final int companyId;

    public Job(int id, String title, Instant timePosted, Category category, int companyId) {
        this.id = id;
        this.title = Objects.requireNonNull(title);
        this.timePosted = Objects.requireNonNull(timePosted);
        this.category = Objects.requireNonNull(category);
        this.companyId = companyId;
    }

    public Job(String title, Category category, Company company) {
        this(jobIds.next(), title, Instant.now(), category, Objects.requireNonNull(company).id);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Job job = (Job) o;
        return title.equals(job.title) &&
                timePosted.equals(job.timePosted) &&
                category == job.category &&
                companyId == job.companyId;
    }

    @Override
    public int hashCode() {
        return Objects.hash(title, timePosted, category, companyId);
    }
}
