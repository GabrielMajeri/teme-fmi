package jobs.db.impl;

import jobs.model.Company;
import jobs.db.JobDatabase;
import jobs.model.JobPosting;

import java.util.ArrayList;
import java.util.List;

public final class InMemoryJobDatabase implements JobDatabase {
    private final List<Company> companyList = new ArrayList<>();
    private final List<JobPosting> jobPostingList = new ArrayList<>();

    @Override
    public void addCompany(Company company) {
        companyList.add(company);
    }

    @Override
    public List<Company> getCompanies() {
        return companyList;
    }

    @Override
    public Company findCompanyByName(String name) {
        for (Company company : companyList) {
            if (company.getName().equals(name)) {
                return company;
            }
        }
        return null;
    }

    @Override
    public void addJob(JobPosting jobPosting) {
        jobPostingList.add(jobPosting);
    }

    @Override
    public List<JobPosting> getJobPostings() {
        return jobPostingList;
    }

    @Override
    public String toString() {
        return "InMemoryJobDatabase{" +
                "companyList=" + companyList +
                '}';
    }
}
