package jobs.model;

import csv.CsvTypeFactory;

import java.util.Date;
import java.util.Objects;

public class Job {
    private final String title;
    private final Date datePosted;
    private final Category category;
    private final Company company;

    public Job(String title, Date datePosted, Category category, Company company) {
        Objects.requireNonNull(title);
        Objects.requireNonNull(datePosted);
        Objects.requireNonNull(category);
        Objects.requireNonNull(company);

        this.title = title;
        this.datePosted = datePosted;
        this.category = category;
        this.company = company;
    }

    public String getTitle() {
        return title;
    }

    public Date getDatePosted() {
        return new Date(datePosted.getTime());
    }

    public Category getCategory() {
        return category;
    }

    public Company getCompany() {
        return company;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Job job = (Job) o;
        return title.equals(job.title) &&
                datePosted.equals(job.datePosted) &&
                category == job.category &&
                company.equals(job.company);
    }

    @Override
    public int hashCode() {
        return Objects.hash(title, datePosted, category, company);
    }
}
