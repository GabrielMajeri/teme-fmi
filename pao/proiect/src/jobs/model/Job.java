package jobs.model;

import csv.CsvTypeFactory;
import jobs.utils.IdAllocator;

import java.time.Instant;
import java.util.Objects;

/**
 * A job offering posted by a company.
 */
public class Job {
    public final static CsvTypeFactory<Job> FACTORY = new CsvTypeFactory<Job>() {
        @Override
        public String[] getColumnNames() {
            return new String[]{"id", "title", "timePosted", "category", "companyId"};
        }

        @Override
        public String[] toStringArray(Job job) {
            return new String[]{
                    Integer.toString(job.id),
                    job.title,
                    job.timePosted.toString(),
                    job.category.toString(),
                    Integer.toString(job.companyId)
            };
        }

        @Override
        public Job fromStringArray(String[] data) {
            int id = Integer.parseInt(data[0]);
            String title = data[1];
            Instant timePosted = Instant.parse(data[2]);
            Category category = Category.valueOf(data[3]);
            int companyId = Integer.parseInt(data[4]);
            return new Job(id, title, timePosted, category, companyId);
        }
    };
    private final static IdAllocator jobIds = new IdAllocator(1, 15000);
    /**
     * Unique identifier of the job.
     */
    public final int id;
    /**
     * Human-readable description of the job position.
     */
    public final String title;
    /**
     * The time it was first published.
     */
    public final Instant timePosted;
    /**
     * The kind of job offered.
     */
    public final Category category;
    /**
     * The unique identifier of the employer.
     */
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
