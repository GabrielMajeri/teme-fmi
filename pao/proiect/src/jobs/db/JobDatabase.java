package jobs.db;

import jobs.model.Company;
import jobs.model.JobPosting;

import java.util.List;

public interface JobDatabase {
    void addCompany(Company company);
    List<Company> getCompanies();

    Company findCompanyByName(String name);

    void addJob(JobPosting jobPosting);

    List<JobPosting> getJobPostings();
}
