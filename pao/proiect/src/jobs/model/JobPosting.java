package jobs.model;

import java.util.Date;

public class JobPosting {
    private String title;
    private Date datePosted;
    private Category category;
    private Company company;

    public JobPosting(String title, Date datePosted, Category category, Company company) {
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
}
