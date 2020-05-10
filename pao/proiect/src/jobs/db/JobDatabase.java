package jobs.db;

import jobs.model.Company;
import jobs.model.Job;

import java.util.Collection;

public interface JobDatabase {
    void addCompany(Company company);
    Collection<Company> getCompanies();
    Company findCompanyByName(String name);

    void addJob(Job job);
    Collection<Job> getJobs();
}
